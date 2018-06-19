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
    )
import           Control.Monad.STM
import           Data.Aeson
    ( Value (String)
    , toJSON
    )
import           Data.ByteString.Char8         (pack)
import qualified Data.HashMap.Strict           as HM
    ( insert
    )
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

trade :: BotState -> (Double, Double) -> BitMEXReader IO ()
trade botState@(BotState {..}) (bestAsk, bestBid) = do
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
                    trade botState (bestAsk, bestBid)
                True -> do
                    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                        makeMarket bestAsk newBestBid
                    let HTTP.Status {statusCode = code} =
                            responseStatus resp
                    if code == 200
                        then trade
                                 botState
                                 (bestAsk, newBestBid)
                        else fail "order didn't go through"
                    -- liftIO $
                    --     atomically $
                    --     waitTilProcessed botState
        True -> do
            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                makeMarket newBestAsk bestBid
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then trade botState (newBestAsk, bestBid)
                else fail "order didn't go through"
            -- liftIO $ atomically $ waitTilProcessed botState
            -- _ <-
            --     liftIO $
            --     atomically $ readResponse orderQueue

tradeLoop :: BotState -> BitMEXApp IO ()
tradeLoop botState@(BotState {..}) conn = do
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
        stopLossBuy =
            prepareOrder
                "buystoploss"
                ("buyteststop" <> (T.pack . show) time)
                StopLimit
                Sell
                0.5
                (Just 1)
                (Just LastPrice)
                (Just OCO)
        stopLossSell =
            prepareOrder
                "sellstoploss"
                ("sellteststop" <> (T.pack . show) time)
                StopLimit
                Buy
                1000000
                (Just 99999)
                (Just LastPrice)
                (Just OCO)
    placeBulkOrder [stopLossBuy, stopLossSell]
    slm <- liftIO $ atomically $ readTVar stopLossMap
    liftIO $
        atomically $
        writeTVar stopLossMap $
        HM.insert
            "SELL"
            ("buyteststop" <> (T.pack . show) time) $
        HM.insert
            "BUY"
            ("buyteststop" <> (T.pack . show) time)
            slm
    check <- liftIO $ atomically $ readTVar stopLossMap
    print check
    trade botState (head $ head obAsks, head $ head obBids)

-- botLoop :: BotState -> BitMEXApp IO ()
-- botLoop botState@(BotState {..}) conn = do
--     wrapperConfig <- R.ask
--     -- forever $ tradeLoop botState conn
--     tradeLoop botState conn
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
    -- R.runReaderT (runBot botLoop) state
    tradeLoop botState conn
