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

data BotState = BotState
    { connection    :: !Connection
    , positionQueue :: !(TQueue (Maybe Response))
    , obQueue       :: !(TQueue (Maybe Response))
    , orderQueue    :: !(TQueue (Maybe Response))
    , marginQueue   :: !(TQueue (Maybe Response))
    , messageQueue  :: !(TQueue (Maybe Response))
    }

newtype BitMEXBot m a = BitMEXBot
    { runBot :: (R.ReaderT BotState (BitMEXReader m) a)
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , R.MonadReader BotState
               )

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

sanityCheck :: STM ()
sanityCheck = return ()

tradeLoop :: BitMEXBot IO ()
tradeLoop = tradeLoop

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x

botLoop :: BitMEXBot IO ()
botLoop = do
    wrapperConfig <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    liftIO $ do
        forkIO $
            forever $ do
                msg <- getMessage connection wrapperConfig
                atomically $ processResponse botState msg
    M (TABLE {_data = marginData}) <-
        liftIO $ atomically $ readResponse marginQueue
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $ atomically $ readResponse obQueue
    let RespMargin {amount = marginAmount} = head marginData
        RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    case marginAmount of
        Nothing ->
            fail "Can't start trading: insufficient funds"
        Just a -> forever $ tradeLoop

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
    let state =
            BotState
            { connection = conn
            , positionQueue = positionQueue
            , obQueue = obQueue
            , orderQueue = orderQueue
            , marginQueue = marginQueue
            , messageQueue = messageQueue
            }
    R.runReaderT (runBot botLoop) state

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
    connect config initBot
