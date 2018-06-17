module Main where

import           BasicPrelude                  hiding (head)
import qualified BitMEX                        as Mex

import           BitMEXClient
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TQueue
import qualified Control.Monad.Reader          as R
    ( MonadReader
    , ReaderT
    , ask
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
import qualified Data.ByteString               as B
    ( readFile
    )
import           Data.ByteString.Char8         (pack)
import qualified Data.Text                     as T (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                   (head)
import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets            (Connection)
import qualified System.Environment            as Env
    ( getArgs
    )

tickSize = 0.5

strategyThreshold = 0.5

minPos = -2

maxPos = 2

data BotState = BotState
    { connection    :: !Connection
    , positionQueue :: !(TQueue (Maybe Response))
    , obQueue       :: !(TQueue (Maybe Response))
    , orderQueue    :: !(TQueue (Maybe Response))
    , marginQueue   :: !(TQueue (Maybe Response))
    , messageQueue  :: !(TQueue (Maybe Response))
    }

-- newtype BitMEXBot m a = BitMEXBot
--     { runBot :: (R.ReaderT BotState (BitMEXReader m) a)
--     } deriving ( Applicative
--                , Functor
--                , Monad
--                , MonadIO
--                , R.MonadReader BotState
--                )
processResponse :: BotState -> Maybe Response -> STM ()
processResponse (BotState {..}) msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 t ->
                    writeTQueue obQueue (Just (OB10 t))
                P t ->
                    writeTQueue positionQueue (Just (P t))
                O t -> writeTQueue orderQueue (Just (O t))
                M t -> writeTQueue marginQueue (Just (M t))
                x -> writeTQueue messageQueue (Just x)

placeBulkOrder ::
       [Mex.Order]
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
placeBulkOrder orders = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNewBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orders\": " <> encode orders <> "}"
    makeRequest orderRequest

placeOrder ::
       Mex.Order
    -> BitMEXReader IO (Mex.MimeResult Mex.Order)
placeOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNew
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) XBTUSD))
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    makeRequest orderRequest

prepareOrder ::
       Text
    -> Text
    -> Side
    -> Double
    -> Maybe Double
    -> Maybe Text
    -> Maybe Text
    -> Mex.Order
prepareOrder id orderType side price stopPx executionType contingencyType =
    (Mex.mkOrder id)
    { Mex.orderSymbol = Just ((T.pack . show) XBTUSD)
    , Mex.orderOrdType = Just orderType
    , Mex.orderClOrdLinkId = Just id
    , Mex.orderSide = Just ((T.pack . show) side)
    , Mex.orderPrice = Just price
    , Mex.orderStopPx = stopPx
    , Mex.orderExecInst = executionType
    , Mex.orderContingencyType = contingencyType
    , Mex.orderOrderQty = Just 66
    }
    -- trade botState (askPrice, bidPrice)

trade :: BotState -> (Double, Double) -> BitMEXReader IO ()
trade botState@(BotState {..}) (bestAsk, bestBid) = do
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse obQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    case newBestAsk /= bestAsk of
        False ->
            case newBestBid /= bestBid of
                False -> do
                    print
                        "==============NOTHING CHANGED==============="
                    trade botState (bestAsk, bestBid)
                True -> do
                    print
                        "==============BID CHANGED=============="
                    trade botState (bestAsk, newBestBid)
        True -> do
            print "==============ASK CHANGED=============="
            trade botState (newBestAsk, bestBid)

tradeLoop :: BotState -> BitMEXApp IO ()
tradeLoop botState@(BotState {..}) conn = do
    M (TABLE {_data = marginData}) <-
        liftIO $ atomically $ readResponse marginQueue
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse obQueue
    let RespMargin {marginBalance = marginAmount} =
            head marginData
        RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        buyOrder =
            prepareOrder
                "buytest"
                "Limit"
                Buy
                (head $ head obBids)
                Nothing
                Nothing
                (Just "OneCancelsTheOther")
        sellOrder =
            prepareOrder
                "selltest"
                "Limit"
                Sell
                (head $ head obAsks)
                Nothing
                Nothing
                (Just "OneCancelsTheOther")
        stopLossBuy =
            prepareOrder
                "buytest"
                "StopLimit"
                Sell
                ((head $ head obBids) - 10.0)
                (Just ((head $ head obBids) - 9.0))
                (Just "LastPrice")
                (Just "OneCancelsTheOther")
        stopLossSell =
            prepareOrder
                "selltest"
                "StopLimit"
                Buy
                ((head $ head obAsks) + 10.0)
                (Just ((head $ head obAsks) + 9.0))
                (Just "LastPrice")
                (Just "OneCancelsTheOther")
    _ <- placeBulkOrder
        [buyOrder, sellOrder, stopLossBuy, stopLossSell]
    return ()

    -- trade botState (head $ head obAsks, head $ head obBids)

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x

botLoop :: BotState -> BitMEXApp IO ()
botLoop botState@(BotState {..}) conn = do
    wrapperConfig <- R.ask
    -- forever $ tradeLoop botState conn
    tradeLoop botState conn

initBot :: BitMEXApp IO ()
initBot conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    positionQueue <- liftIO $ atomically newTQueue
    obQueue <- liftIO $ atomically newTQueue
    orderQueue <- liftIO $ atomically newTQueue
    marginQueue <- liftIO $ atomically newTQueue
    messageQueue <- liftIO $ atomically newTQueue
    let botState =
            BotState
            { connection = conn
            , positionQueue = positionQueue
            , obQueue = obQueue
            , orderQueue = orderQueue
            , marginQueue = marginQueue
            , messageQueue = messageQueue
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
    botLoop botState conn

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- Env.getArgs
    pub <- readFile pubPath
    priv <- B.readFile privPath
    logCxt <- Mex.initLogContext
    let config0 =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    config <- return config0 >>= withStdoutLoggingWS
    connect config initBot
