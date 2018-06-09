module Main where

import           BasicPrelude                  hiding (head)
import           BitMEX
    ( initLogContext
    , runDefaultLogExecWithContext
    )

import           BitMEXClient
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TQueue
import qualified Control.Monad.Reader          as R
    ( ask
    , asks
    )
import           Control.Monad.STM
import           Data.Aeson
    ( Value (String)
    , decode
    , toJSON
    )
import qualified Data.ByteString               as B
    ( readFile
    )
import           Data.ByteString.Char8         (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                   (head, (!))
import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( Connection
    , receiveData
    , sendClose
    , sendTextData
    )
import qualified System.Environment            as Env
    ( getArgs
    )

tickSize = 0.5

strategyThreshold = 0.5

minPos = -2

maxPos = 2

positionThread :: TQueue a -> STM ()
positionThread = undefined

orderThread :: TQueue (Maybe Response) -> STM Double
orderThread q = do
    o <- readTQueue q
    case o of
        Nothing -> retry
        Just (OB10 (TABLE {..})) -> do
            let RespOrderBook10 {..} = head _data
                bestAskPrice = head $ head asks
            return bestAskPrice

writeOrders ::
       Maybe Response -> TQueue (Maybe Response) -> STM ()
writeOrders msg q = do
    case msg :: Maybe Response of
        Nothing -> return ()
        Just r ->
            case r of
                (OB10 t) -> writeTQueue q (Just (OB10 t))
                _        -> return ()

processResponse ::
       Maybe Response
    -> TQueue (Maybe Response)
    -> TQueue (Maybe Response)
    -> TQueue (Maybe Response)
    -> STM ()
processResponse msg positionQueue orderQueue messageQueue =
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 t ->
                    writeTQueue orderQueue (Just (OB10 t))
                P t ->
                    writeTQueue positionQueue (Just (P t))
                x -> writeTQueue messageQueue (Just x)

logResponse ::
       TQueue (Maybe Response) -> TQueue String -> STM ()
logResponse q logQueue = do
    x <- readTQueue q
    case x of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 (TABLE {..}) -> do
                    let RespOrderBook10 {..} = head _data
                        bestAskPrice = head $ head asks
                        bestAskSize = head asks ! 1
                        bestBidPrice = head $ head bids
                        bestBidSize = head bids ! 1
                    writeTQueue logQueue $ show bestAskPrice
                P (TABLE {..}) -> do
                    let RespPosition {..} = head _data
                        msg =
                            "pos info: " ++
                            show account ++
                            " " ++
                            show symbol ++
                            " " ++ show currency
                    writeTQueue logQueue msg
                Status (STATUS {..}) ->
                    writeTQueue logQueue $ show subscribe
                Info (INFO {..}) ->
                    writeTQueue logQueue $ show info
                x -> writeTQueue logQueue $ show x

retrieveLogs :: TQueue String -> STM String
retrieveLogs q = do
    readTQueue q >>= return

tradeLoop :: BitMEXApp ()
tradeLoop conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    liftIO $ do
        positionQueue <- atomically newTQueue
        orderQueue <- atomically newTQueue
        messageQueue <- atomically newTQueue
        logQueue <- atomically newTQueue
        sendMessage
            conn
            AuthKey
            [String pub, toJSON time, (toJSON . show) sig]
    -- _ <- forkIO $ sanityCheck 0.0 0.0
        sendMessage
            conn
            Subscribe
            ([ OrderBook10 XBTUSD
             , Execution
             , Order
             , Position
             ] :: [Topic Symbol])
        forever $ do
            msg <- getMessage conn config
            atomically $
                processResponse
                    msg
                    positionQueue
                    orderQueue
                    messageQueue

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- Env.getArgs
    pub <- readFile pubPath
    priv <- B.readFile privPath
    logCxt <- initLogContext
    let config0 =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext = runDefaultLogExecWithContext
            , logContext = logCxt
            }
    config <- return config0 >>= withStdoutLoggingWS
    connect config tradeLoop
