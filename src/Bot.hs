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
import qualified Data.HashMap.Strict           as HM
    ( lookup
    )
import qualified Data.HashMap.Strict           as HM
    ( insert
    )
import           Data.Maybe                    (fromJust)
import qualified Data.Text                     as T (pack)
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

trade :: (Double, Double) -> BitMEXBot IO ()
trade (bestAsk, bestBid) = do
    botState@(BotState {..}) <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    case newBestAsk /= bestAsk of
        False ->
            case newBestBid /= bestBid of
                False -> do
                    trade (bestAsk, bestBid)
                True -> do
                    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                        makeMarket bestAsk newBestBid
                    let HTTP.Status {statusCode = code} =
                            responseStatus resp
                    if code == 200
                        then trade (bestAsk, newBestBid)
                        else fail "order didn't go through"
        True -> do
            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                makeMarket newBestAsk bestBid
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then trade (newBestAsk, bestBid)
                else fail "order didn't go through"

tradeLoop :: BitMEXBot IO ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    M (TABLE {_data = marginData}) <-
        liftIO $ atomically $ readResponse marginQueue
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse lobQueue
    let RespMargin {marginBalance = marginAmount} =
            head marginData
        RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    -- Setup placeholder stoploss orders
    initStopLossOrders time
    _ <-
        liftIO $
        forkIO $ forever $ do riskLoop botState config
    slmap <- liftIO $ atomically $ readTVar stopLossMap
    let stopLossBuy =
            prepareOrder
                Nothing
                (fromJust $
                 HM.lookup "LONG_POSITION_STOP_LOSS" slmap)
                Nothing
                (Just Sell)
                ((head $ head obBids) - 1000)
                (Just ((head $ head obBids) - 999))
                6
                Nothing
                Nothing
    res <- bulkAmendOrders [stopLossBuy]
    return ()
    -- trade botState (head $ head obAsks, head $ head obBids)

riskLoop :: BotState -> BitMEXWrapperConfig -> IO ()
riskLoop botState@BotState {..} config = do
    (qty, price) <- atomically $ riskManager positionQueue
    slm <- atomically $ readTVar stopLossMap
    R.runReaderT
        (run (R.runReaderT
                  (runBot (manageRisk qty price))
                  botState))
        config

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
    positionsMap <-
        liftIO $
        atomically $
        newTVar (mempty :: HashMap Text (Double, Double))
    stopLossMap <-
        liftIO $
        atomically $ newTVar (mempty :: HashMap Text Text)
    let botState =
            BotState
            { connection = conn
            , positionQueue = positionQueue
            , lobQueue = lobQueue
            , orderQueue = orderQueue
            , marginQueue = marginQueue
            , executionQueue = executionQueue
            , messageQueue = messageQueue
            , positionsMap = positionsMap
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
