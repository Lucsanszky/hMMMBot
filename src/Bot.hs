module Bot
    ( initBot
    ) where

import           BasicPrelude                  hiding (head)
import qualified BitMEX                        as Mex
import           BitMEXClient
import           Bot.Concurrent
import           Bot.OrderTemplates
import           Bot.RiskManager
import           Bot.Types
import           Bot.Util
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Monad.Reader          as R
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
import           Data.ByteString.Char8         (pack)
import qualified Data.HashMap.Strict           as HM
    ( lookup
    )
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                   (head)
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status     as HTTP
    ( Status (..)
    )

_MAX_POSITION_ :: Int
_MAX_POSITION_ = 42

_ORDER_SIZE_ :: Int
_ORDER_SIZE_ = 21

trade :: (Double, Double) -> BitMEXBot IO ()
trade (bestAsk, bestBid) = do
    BotState {..} <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
            atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    size <- liftIO $ atomically $ readTVar positionSize
    case newBestAsk /= bestAsk || newBestBid /= bestBid of
        False -> do
            trade (bestAsk, bestBid)
        True -> do
            buys' <- liftIO $ atomically $ readTVar openBuys
            sells' <- liftIO $ atomically $ readTVar openSells
            let buys = if size > 0 then size + buys' else buys'
            let sells = if size < 0 then (abs size) + sells' else sells'
            if (buys < 42 || sells < 42)
               then do
                  let orders = if (buys < 42 && sells < 42)
                                  then [limitSell newBestAsk, limitBuy newBestBid]
                                  else if (buys < 42)
                                          then [limitBuy newBestBid]
                                          else [limitSell newBestAsk]
                  Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                      makeMarket orders
                  let HTTP.Status {statusCode = code} =
                          responseStatus resp
                  if code == 200
                      then trade (newBestAsk, newBestBid)
                      else fail "order didn't go through"
                else do
                    trade (newBestAsk, newBestBid)

tradeLoop :: BitMEXBot IO ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    _ <-
        liftIO $
        forkIO $ forever $ riskManager botState config
    _ <-
        liftIO $
        forkIO $ forever $ stopLossWatcher botState config
    _ <- liftIO $ forkIO $ forever $ pnlTracker pnlQueue
    trade (head $ head obAsks, head $ head obBids)

initBot :: BitMEXApp IO ()
initBot conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    lobQueue <- liftIO $ atomically newTQueue
    riskManagerQueue <- liftIO $ atomically newTQueue
    openOrderQueue <- liftIO $ atomically newTQueue
    slwQueue <- liftIO $ atomically newTQueue
    pnlQueue <- liftIO $ atomically newTQueue
    prevPosition <- liftIO $ atomically $ newTVar 0
    positionSize <- liftIO $ atomically $ newTVar 0
    openBuys <- liftIO $ atomically $ newTVar 0
    openSells <- liftIO $ atomically $ newTVar 0
    stopOrderId <- liftIO $ atomically $ newTVar (OrderID Nothing)
    let botState =
            BotState
            { connection = conn
            , lobQueue = LOBQueue lobQueue
            , riskManagerQueue =
                  RiskManagerQueue riskManagerQueue
            , openOrderQueue = OpenOrderQueue openOrderQueue
            , slwQueue = StopLossWatcherQueue slwQueue
            , pnlQueue = PnLQueue pnlQueue
            , prevPosition = prevPosition
            , positionSize = positionSize
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
        forkIO $
            forever $ do
                msg <- getMessage conn config
                processResponse botState msg
    R.runReaderT (runBot tradeLoop) botState
