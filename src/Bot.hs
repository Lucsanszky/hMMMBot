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
import           Data.Vector
    ( head
    , last
    , (!)
    )
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

trader :: BotState -> BitMEXWrapperConfig -> IO ()
trader botState@BotState {..} config =
    unWrapBotWith trade botState config

trade :: BitMEXBot ()
trade = do
    BotState {..} <- R.ask
    obAsks' <- liftIO $ atomically $ readTVar obAsks
    obBids' <- liftIO $ atomically $ readTVar obBids
    when ((not . null) obAsks' && (not . null) obBids') $ do
        total <-
            liftIO $ atomically $ readTVar walletBalance
        buyQty <- liftIO $ atomically $ readTVar openBuys
        buyCost <-
            liftIO $ atomically $ readTVar openBuyCost
        sellQty <- liftIO $ atomically $ readTVar openSells
        sellCost <-
            liftIO $ atomically $ readTVar openSellCost
        let newBestAsk = head $ head obAsks'
            newBestBid = head $ head obBids'
            orderSize =
                getOrderSize newBestAsk $
                fromIntegral total * lev
            lev = Mex.unLeverage leverage
            limit =
                getLimit newBestAsk $
                fromIntegral total * lev
        when (buyQty /= 0 && buyCost /= 0) $ do
            let buyAvg =
                    (fromIntegral buyQty) /
                    convert
                        XBt_to_XBT
                        (fromIntegral buyCost)
            when ((abs buyAvg) < newBestBid - 2.5) $ do
                cancelLimitOrders "Buy"
                makeMarket
                    limit
                    buyQty
                    newBestAsk
                    newBestBid
                return ()
        when (sellQty /= 0 && sellCost /= 0) $ do
            let sellAvg =
                    (fromIntegral sellQty) /
                    convert
                        XBt_to_XBT
                        (fromIntegral sellCost)
            when ((abs sellAvg) > newBestAsk + 2.5) $ do
                cancelLimitOrders "Sell"
                makeMarket
                    limit
                    sellQty
                    newBestAsk
                    newBestBid
                return ()
        available <-
            liftIO $ atomically $ readTVar availableBalance
        if (convert XBt_to_XBT (fromIntegral available)) >
           convert USD_to_XBT newBestAsk *
           (fromIntegral orderSize) /
           lev
            then makeMarket
                     limit
                     orderSize
                     newBestAsk
                     newBestBid
            else do
                kill "not enough funds"

tradeLoop :: BitMEXBot ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    liftIO $ do
        risk <-
            async $ forever $ riskManager botState config
        slw <-
            async $
            forever $ stopLossWatcher botState config
        pnl <- async $ forever $ pnlTracker botState config
        tr <- async $ forever $ trader botState config
        loss <-
            async $
            forever $ lossLimitUpdater botState config
        mapM_ A.link [risk, slw, pnl, tr, loss]
    loop
  where
    loop = loop

initBot :: Mex.Leverage -> BitMEXApp ()
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
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 100
    slwQueue <- liftIO $ atomically $ newTBQueue 100
    pnlQueue <- liftIO $ atomically $ newTBQueue 100
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ atomically $ newTVar 0
    obAsks <- liftIO $ atomically $ newTVar empty
    obBids <- liftIO $ atomically $ newTVar empty
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
            , riskManagerQueue =
                  RiskManagerQueue riskManagerQueue
            , slwQueue = StopLossWatcherQueue slwQueue
            , pnlQueue = PnLQueue pnlQueue
            , prevPosition = prevPosition
            , positionSize = positionSize
            , obAsks = obAsks
            , obBids = obBids
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
             , Position
             , Margin
             ] :: [Topic Symbol])
        async $
            forever $ do
                msg <- getMessage conn config
                processResponse botState msg
    R.runReaderT (runBot tradeLoop) botState
