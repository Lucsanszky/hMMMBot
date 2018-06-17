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
import           Data.Vector                   (head, (!?))
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
    { connection     :: !Connection
    , positionQueue  :: !(TQueue (Maybe Response))
    , lobQueue       :: !(TQueue (Maybe Response))
    , orderQueue     :: !(TQueue (Maybe Response))
    , marginQueue    :: !(TQueue (Maybe Response))
    , executionQueue :: !(TQueue (Maybe Response))
    , messageQueue   :: !(TQueue (Maybe Response))
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
                    writeTQueue lobQueue (Just (OB10 t))
                P t ->
                    writeTQueue positionQueue (Just (P t))
                O t -> writeTQueue orderQueue (Just (O t))
                M t -> writeTQueue marginQueue (Just (M t))
                Exe t ->
                    writeTQueue
                        executionQueue
                        (Just (Exe t))
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
    -> OrderType
    -> Side
    -> Double
    -> Maybe Double
    -> Maybe ExecutionInstruction
    -> Maybe ContingencyType
    -> Mex.Order
prepareOrder linkId clientId orderType side price stopPx executionType contingencyType =
    (Mex.mkOrder clientId)
    { Mex.orderSymbol = Just ((T.pack . show) XBTUSD)
    , Mex.orderOrdType = Just ((T.pack . show) orderType)
    , Mex.orderClOrdLinkId = Just linkId
    , Mex.orderClOrdId = Just clientId
    , Mex.orderSide = Just ((T.pack . show) side)
    , Mex.orderPrice = Just price
    , Mex.orderStopPx = stopPx
    , Mex.orderExecInst = fmap (T.pack . show) executionType
    , Mex.orderContingencyType =
          fmap (T.pack . show) contingencyType
    , Mex.orderOrderQty = Just 66
    }
    -- trade botState (askPrice, bidPrice)

makeMarket ::
       Double
    -> Double
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
makeMarket ask bid = do
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    let buyOrder =
            prepareOrder
                "buytest"
                ("buytestlimit" <> (T.pack . show) time)
                Limit
                Buy
                bid
                Nothing
                Nothing
                Nothing
        sellOrder =
            prepareOrder
                "selltest"
                ("selltestlimit" <> (T.pack . show) time)
                Limit
                Sell
                ask
                Nothing
                Nothing
                Nothing
        stopLossBuy =
            prepareOrder
                "buytest"
                ("buyteststop" <> (T.pack . show) time)
                StopLimit
                Sell
                (bid - 10.0)
                (Just (bid - 9.0))
                (Just LastPrice)
                (Just OCO)
        stopLossSell =
            prepareOrder
                "selltest"
                ("sellteststop" <> (T.pack . show) time)
                StopLimit
                Buy
                (ask + 10.0)
                (Just (ask + 9.0))
                (Just LastPrice)
                (Just OCO)
    placeBulkOrder
        [buyOrder, sellOrder, stopLossBuy, stopLossSell]

waitTilProcessed :: BotState -> STM Bool
waitTilProcessed BotState {..} = do
    O (TABLE {_data = orderData}) <- readResponse orderQueue
    case orderData !? 0 of
        Nothing -> retry
        Just (RespOrder {clOrdID = id}) ->
            case id of
                Nothing -> retry
                Just clientId -> do
                    Exe (TABLE {_data = executionData}) <-
                        readResponse executionQueue
                    case executionData !? 0 of
                        Nothing -> retry
                        Just (RespExecution { clOrdID = respId
                                            , workingIndicator = working
                                            }) ->
                            case respId of
                                Nothing -> retry
                                Just x ->
                                    case (fmap
                                              (&& (x ==
                                                   clientId))
                                              working) of
                                        Just True ->
                                            return True
                                        _ -> retry

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
                    _ <- makeMarket bestAsk newBestBid
                    -- Wait for order to complete
                    liftIO $
                        atomically $
                        waitTilProcessed botState
                    trade botState (bestAsk, newBestBid)
        True -> do
            _ <- makeMarket newBestAsk bestBid
            -- Wait for order to complete
            liftIO $ atomically $ waitTilProcessed botState
            -- _ <-
            --     liftIO $
            --     atomically $ readResponse orderQueue
            trade botState (newBestAsk, bestBid)

tradeLoop :: BotState -> BitMEXApp IO ()
tradeLoop botState@(BotState {..}) conn = do
    M (TABLE {_data = marginData}) <-
        liftIO $ atomically $ readResponse marginQueue
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse lobQueue
    let RespMargin {marginBalance = marginAmount} =
            head marginData
        RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    trade botState (head $ head obAsks, head $ head obBids)

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x

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
    let botState =
            BotState
            { connection = conn
            , positionQueue = positionQueue
            , lobQueue = lobQueue
            , orderQueue = orderQueue
            , marginQueue = marginQueue
            , executionQueue = executionQueue
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
    tradeLoop botState conn

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
