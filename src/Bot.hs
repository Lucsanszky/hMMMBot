module Bot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
    , last
    )
import qualified BitMEX                         as Mex
import           BitMEXClient
import           Bot.Concurrent
import           Bot.Math
import           Bot.OrderTemplates
import           Bot.RiskManager
import           Bot.Types
import           Bot.Util
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (async)
import qualified Control.Concurrent.Async       as A (link)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Monad.Reader           as R
    ( ask
    , asks
    , lift
    , runReaderT
    )
import           Control.Monad.STM
import           Data.Aeson
    ( Value (String)
    , encode
    , toJSON
    )
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                    (head, last)
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

trade :: (Double, Double) -> BitMEXBot IO ()
trade (bestAsk, bestBid) = do
    BotState {..} <- R.ask
    available <-
        liftIO $ atomically $ readTVar availableBalance
    total <- liftIO $ atomically $ readTVar walletBalance
    buyQty <- liftIO $ atomically $ readTVar openBuys
    buyCost <- liftIO $ atomically $ readTVar openBuyCost
    sellQty <- liftIO $ atomically $ readTVar openSells
    sellCost <- liftIO $ atomically $ readTVar openSellCost
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        worstAsk = head $ last obAsks
        newBestBid = head $ head obBids
        worstBid = head $ last obBids
        sellVol = (foldl' (+) 0 . map last) $ obAsks
        buyVol = (foldl' (+) 0 . map last) $ obBids
        imbalance =
            (abs (sellVol - buyVol)) / sellVol + buyVol
        orderSize =
            getOrderSize newBestAsk $ fromIntegral total
        lev = Mex.unLeverage leverage
        aggressiveLimit =
            getAggressiveLimit newBestAsk $
            fromIntegral total / lev
        passiveLimit =
            getPassiveLimit newBestAsk $
            fromIntegral total / lev
    when (buyQty /= 0 && buyCost /= 0) $ do
        let buyAvg =
                (fromIntegral buyQty) /
                convert
                    XBt_to_XBT
                    (fromIntegral buyCost)
        when ((abs buyAvg) < worstBid) $ do
            cancelLimitOrders "Buy"
            trade (newBestAsk, newBestBid)
    when (sellQty /= 0 && sellCost /= 0) $ do
        let sellAvg =
                (fromIntegral sellQty) /
                convert
                    XBt_to_XBT
                    (fromIntegral sellCost)
        when ((abs sellAvg) > worstAsk) $ do
            cancelLimitOrders "Sell"
            trade (newBestAsk, newBestBid)
    if (convert XBt_to_XBT (fromIntegral available)) >
        convert USD_to_XBT newBestAsk *
        (fromIntegral orderSize) *
        lev
        then if imbalance > 0.5 &&
                (newBestAsk - newBestBid > 1.0)
                  then do
                      let avg =
                              (newBestAsk +
                              newBestBid) /
                              2
                      if (buyVol > sellVol)
                          then do
                              makeMarket
                                  aggressiveLimit
                                  orderSize
                                  (fromIntegral $
                                  ceiling avg)
                                  ((fromIntegral $
                                    ceiling avg) -
                                  0.5)
                              trade
                                  ( (fromIntegral $
                                    ceiling avg)
                                  , (fromIntegral $
                                    ceiling avg) -
                                    0.5)
                          else do
                              makeMarket
                                  aggressiveLimit
                                  orderSize
                                  ((fromIntegral $
                                    floor avg) +
                                  0.5)
                                  (fromIntegral $
                                  floor avg)
                              trade
                                  ( (fromIntegral $
                                    floor avg) +
                                    0.5
                                  , (fromIntegral $
                                    floor avg))
                  else do
                      makeMarket
                          passiveLimit
                          orderSize
                          newBestAsk
                          newBestBid
                      trade (newBestAsk, newBestBid)
        else do
            kill "not enough funds"

tradeLoop :: BitMEXBot IO ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    liftIO $ do
        risk <-
            async $ forever $ riskManager botState config
        slw <-
            async $
            forever $ stopLossWatcher botState config
        pnl <- async $ forever $ pnlTracker botState config
        mapM_ A.link [risk, slw, pnl]
    trade (head $ head obAsks, head $ head obBids)

initBot :: Mex.Leverage -> BitMEXApp IO ()
initBot leverage conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    Mex.MimeResult {Mex.mimeResult = res} <-
        makeRequest $
        Mex.userGetMargin (Mex.Accept Mex.MimeJSON)
    let Right (Mex.Margin { Mex.marginWalletBalance = Just wb
                          , Mex.marginAvailableMargin = Just ab
                          }) = res
    lobQueue <- liftIO $ atomically $ newTBQueue 9
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 9
    slwQueue <- liftIO $ atomically $ newTBQueue 9
    pnlQueue <- liftIO $ atomically $ newTBQueue 9
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ atomically $ newTVar 0
    realPnl <- liftIO $ atomically $ newTVar 0
    prevBalance <- liftIO $ atomically $ newTVar $ floor wb
    availableBalance <-
        liftIO $ atomically $ newTVar $ floor ab
    walletBalance <-
        liftIO $ atomically $ newTVar $ floor wb
    openBuys <- liftIO $ atomically $ newTVar 0
    openBuyCost <- liftIO $ atomically $ newTVar 0
    openSells <- liftIO $ atomically $ newTVar 0
    openSellCost <- liftIO $ atomically $ newTVar 0
    stopOrderId <-
        liftIO $ atomically $ newTVar (OrderID Nothing)
    let botState =
            BotState
            { connection = conn
            , lobQueue = LOBQueue lobQueue
            , riskManagerQueue =
                  RiskManagerQueue riskManagerQueue
            , slwQueue = StopLossWatcherQueue slwQueue
            , pnlQueue = PnLQueue pnlQueue
            , prevPosition = prevPosition
            , positionSize = positionSize
            , realPnl = realPnl
            , prevBalance = prevBalance
            , availableBalance = availableBalance
            , walletBalance = walletBalance
            , openBuys = openBuys
            , openBuyCost = openBuyCost
            , openSells = openSells
            , openSellCost = openSellCost
            , stopOrderId = stopOrderId
            , leverage = leverage
            }
    _ <- updateLeverage XBTUSD leverage
    liftIO $ do
        sendMessage
            conn
            AuthKey
            [String pub, toJSON time, (toJSON . show) sig]
        sendMessage
            conn
            Subscribe
            ([ OrderBook10 XBTUSD
             , Execution
             , Order
             , Position
             , Margin
             ] :: [Topic Symbol])
        async $
            forever $ do
                msg <- getMessage conn config
                processResponse botState msg
    R.runReaderT (runBot tradeLoop) botState
