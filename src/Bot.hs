module Bot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
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
    , toJSON
    )
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                    (head)
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

_MAX_POSITION_ :: Integer
_MAX_POSITION_ = 42

_ORDER_SIZE_ :: Int
_ORDER_SIZE_ = 21

trade :: (Double, Double) -> BitMEXBot IO ()
trade (bestAsk, bestBid) = do
    BotState {..} <- R.ask
    available <- liftIO $ atomically $ readTVar availableBalance
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    case newBestAsk /= bestAsk || newBestBid /= bestBid of
        False -> do
            trade (bestAsk, bestBid)
        True -> do
            if (convert XBt_to_XBT (fromIntegral available)) > convert USD_to_XBT newBestAsk * 21
               then do
                  makeMarketPassive newBestAsk newBestBid
                  trade (newBestAsk, newBestBid)
               else do
                  kill
                  fail "not enough funds"

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

initBot :: BitMEXApp IO ()
initBot conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    Mex.MimeResult {Mex.mimeResult = res } <- makeRequest $ Mex.userGetMargin (Mex.Accept Mex.MimeJSON)
    let Right (Mex.Margin { Mex.marginMarginBalance = Just mb
                          , Mex.marginWalletBalance = Just wb
                          , Mex.marginAvailableMargin = Just ab
                          }) = res
    lobQueue <- liftIO $ atomically $ newTBQueue 1
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 1
    slwQueue <- liftIO $ atomically $ newTBQueue 1
    pnlQueue <- liftIO $ atomically $ newTBQueue 1
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ atomically $ newTVar 0
    realPnl <- liftIO $ atomically $ newTVar 0
    startingBalance <- liftIO $ atomically $ newTVar $ floor mb
    availableBalance <- liftIO $ atomically $ newTVar $ floor ab
    walletBalance <- liftIO $ atomically $ newTVar $ floor wb
    openBuys <- liftIO $ atomically $ newTVar 0
    openSells <- liftIO $ atomically $ newTVar 0
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
            , startingBalance = startingBalance
            , availableBalance = availableBalance
            , walletBalance = walletBalance
            , openBuys = openBuys
            , openSells = openSells
            , stopOrderId = stopOrderId
            }
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
