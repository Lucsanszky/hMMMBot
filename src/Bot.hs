module Bot
    ( initBot
    ) where

import           BasicPrelude                  hiding (head)
import qualified BitMEX                        as Mex
import           BitMEXClient
import           Bot.Concurrent
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
        liftIO $ atomically $ readResponse lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    size <- liftIO $ atomically $ readTVar positionSize
    case newBestAsk /= bestAsk of
        False ->
            case newBestBid /= bestBid of
                False -> do
                    trade (bestAsk, bestBid)
                True -> do
                    if (_MAX_POSITION_ - (abs size) -
                        _ORDER_SIZE_ >=
                        0)
                        then do
                            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                                makeMarket
                                    bestAsk
                                    newBestBid
                            let HTTP.Status {statusCode = code} =
                                    responseStatus resp
                            if code == 200
                                then trade
                                         ( bestAsk
                                         , newBestBid)
                                else fail
                                         "order didn't go through"
                        else do
                            trade (bestAsk, newBestBid)
        True -> do
            if (_MAX_POSITION_ - (abs size) - _ORDER_SIZE_ >=
                0)
                then do
                    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                        makeMarket newBestAsk bestBid
                    let HTTP.Status {statusCode = code} =
                            responseStatus resp
                    if code == 200
                        then trade (newBestAsk, bestBid)
                        else fail "order didn't go through"
                else do
                    trade (newBestAsk, bestBid)

tradeLoop :: BitMEXBot IO ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    M (TABLE {_data = marginData}) <-
        liftIO $ atomically $ readResponse marginQueue
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse lobQueue
    let RespMargin {marginBalance = marginAmount} =
            head marginData
        RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    -- Setup placeholder stoploss orders
    initStopLossOrders
    _ <-
        liftIO $
        forkIO $ forever $ positionTracker botState config
    _ <-
        liftIO $
        forkIO $ forever $ riskManager botState config
    _ <-
        liftIO $
        forkIO $ forever $ stopLossWatcher botState config

    trade (head $ head obAsks, head $ head obBids)

initBot :: BitMEXApp IO ()
initBot conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    positionQueue <- liftIO $ atomically newTQueue
    lobQueue <- liftIO $ atomically newTQueue
    orderQueue <- liftIO $ atomically newTQueue
    marginQueue <- liftIO $ atomically newTQueue
    executionQueue <- liftIO $ atomically newTQueue
    messageQueue <- liftIO $ atomically newTQueue
    positionSize <- liftIO $ atomically $ newTVar 0
    stopLossMap <-
        liftIO $
        atomically $ newTVar (mempty :: HashMap Text (Text, Double))
    let botState =
            BotState
            { connection = conn
            , positionQueue = positionQueue
            , lobQueue = lobQueue
            , orderQueue = orderQueue
            , marginQueue = marginQueue
            , executionQueue = executionQueue
            , messageQueue = messageQueue
            , positionSize = positionSize
            , stopLossMap = stopLossMap
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
                atomically $ processResponse botState msg
    R.runReaderT (runBot tradeLoop) botState
