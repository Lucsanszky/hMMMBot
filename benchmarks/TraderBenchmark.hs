module Main where

import           BasicPrelude                   hiding
    ( getArgs
    , readFile
    )
import qualified BitMEX                         as Mex
import qualified BitMEX                         as Mex
    ( Accept (..)
    , Leverage (..)
    , Margin (..)
    , MimeJSON (..)
    , MimeResult (..)
    , userGetMargin
    )
import           BitMEXClient
import           BitMEXClient
    ( BitMEXApp
    , BitMEXWrapperConfig (..)
    , Command (..)
    , Symbol (..)
    , Topic (..)
    , getMessage
    , makeRequest
    , makeTimestamp
    , sendMessage
    , sign
    , withConnectAndSubscribe
    )
import           Bot.Concurrent
    ( processResponse
    )
import Bot.Logging
import           Bot.OrderTemplates
import           Bot.RiskManager
    ( lossLimitUpdater
    , pnlTracker
    , riskManager
    , stopLossWatcher
    )
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    , OrderID (..)
    , PnLQueue (..)
    , PositionType (..)
    , RiskManagerQueue (..)
    , StopLossWatcherQueue (..)
    )
import           Bot.Util
import           Bot.Util
    ( updateLeverage
    )
import           Control.Concurrent.Async
    ( async
    , waitAnyCatch
    , waitCatch
    )
import qualified Control.Concurrent.Async       as A (link)
import           Control.Concurrent.STM.TBQueue (newTBQueue)
import           Control.Concurrent.STM.TVar    (newTVar)
import qualified Control.Monad.Reader           as R
import qualified Control.Monad.Reader           as R
    ( ask
    , asks
    , lift
    , runReaderT
    )
import           Control.Monad.STM              (atomically)
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Data.Aeson
import           Data.Aeson
    ( Value (String)
    , toJSON
    )
import           Data.ByteString                (readFile)
import           Data.ByteString.Char8          (pack)
import qualified Data.ByteString.Lazy.Char8     as LBC
    ( unpack
    )
import           Data.IORef                     (newIORef)
import           Data.Monoid
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import Data.Time.Clock (getCurrentTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.Socket
    ( withSocketsDo
    )
import           Network.WebSockets             (Connection, receiveData)
import           System.Environment
    ( getArgs
    , withArgs
    )
import           Wuss
    ( runSecureClient
    )

-- tradeLoop :: BitMEXBot ()
-- tradeLoop = do
--     config <- BitMEXBot $ R.lift R.ask
--     botState@BotState {..} <- R.ask
--     liftIO $ do
--         risk <-
--             async $ forever $ riskManager botState config
--         slw <-
--             async $
--             forever $ stopLossWatcher botState config
--         -- pnl <- async $ forever $ pnlTracker botState config
--         loss <-
--             async $
--             forever $ lossLimitUpdater botState config
--         mapM_ A.link [risk, slw, loss]
--     loop
--   where
--     loop = loop
botBenchmark :: Connection -> BitMEXBot ()
botBenchmark conn = do
    botState@BotState {..} <- R.ask
    config <- BitMEXBot $ lift $ R.ask
    -- _ <- liftIO $ async $ unWrapBotWith tradeLoop botState config
    liftIO $ do
        withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage" $
                  nfIO $ do
                      msg <- getMessage conn config
                      return ()
                      -- processor <-
                      --     async $
                      --     forever $ do
                      --         msg <- getMessage conn config
                      --         return ()
                          -- processResponse
                          --     msg
                          --     botState
                          --     config
                -- unWrapBotWith (placeBulkOrder [limitBuy Nothing 6 666] 6 666 666) botState config
                ]

initBotState :: Mex.Leverage -> BitMEXReader (BotState)
initBotState lev = do
    Mex.MimeResult {Mex.mimeResult = res} <-
        makeRequest $
        Mex.userGetMargin (Mex.Accept Mex.MimeJSON)
    let Right Mex.Margin { Mex.marginWalletBalance = Just wb
                         , Mex.marginAvailableMargin = Just ab
                         } = res
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 1
    slwQueue <- liftIO $ atomically $ newTBQueue 1
    pnlQueue <- liftIO $ atomically $ newTBQueue 1
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ newIORef 0
    realPnl <- liftIO $ atomically $ newTVar 0
    prevBalance <- liftIO $ atomically $ newTVar $ floor wb
    availableBalance <-
        liftIO $ atomically $ newTVar $ floor ab
    walletBalance <-
        liftIO $ atomically $ newTVar $ floor wb
    openBuys <- liftIO $ newIORef 0
    openBuyCost <- liftIO $ newIORef 0
    openSells <- liftIO $ newIORef 0
    openSellCost <- liftIO $ newIORef 0
    bestBid <- liftIO $ newIORef 0.0
    bestAsk <- liftIO $ newIORef 99999999999.0
    sellID <- liftIO $ newIORef (OrderID Nothing)
    buyID <- liftIO $ newIORef (OrderID Nothing)
    stopOrderId <-
        liftIO $ atomically $ newTVar (OrderID Nothing)
    _ <- updateLeverage XBTUSD lev
    return
        (BotState
         { riskManagerQueue =
               RiskManagerQueue riskManagerQueue
         , slwQueue = StopLossWatcherQueue slwQueue
         , pnlQueue = PnLQueue pnlQueue
         , prevPosition = prevPosition
         , positionSize = positionSize
         , realPnl = realPnl
         , prevBalance = prevBalance
         , availableBalance = availableBalance
         , walletBalance = walletBalance
         , bestAsk = bestAsk
         , bestBid = bestBid
         , openBuys = openBuys
         , openBuyCost = openBuyCost
         , openSells = openSells
         , openSellCost = openSellCost
         , buyID = buyID
         , sellID = sellID
         , stopOrderId = stopOrderId
         , leverage = lev
         })

initBot :: Mex.Leverage -> BitMEXReader ()
initBot lev = do
    config@BitMEXWrapperConfig {..} <- R.ask
    botState <- initBotState lev
    liftIO $
        withConnectAndSubscribe config [OrderBook10 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 180})
                [ bench "getMessage with OB10" $
                  nfIO $ do
                      msg <- receiveData c
                      getCurrentTime >>= print
                      print (msg :: T.Text)
                      return ()
                ]
    liftIO $
        withConnectAndSubscribe
            config
            [OrderBook10 XBTUSD, OrderBookL2 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OB10 and OBL2" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    ob10 <-
        liftIO $
        async $
        withConnectAndSubscribe config [OrderBook10 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OB10 inside async" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    obl2 <-
        liftIO $
        async $
        withConnectAndSubscribe config [OrderBookL2 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OBL2 inside async" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    eres <- liftIO $ waitAnyCatch [ob10, obl2]
    case eres of
        (_, Right _) -> return ()
        (_, Left _)  -> return ()

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:esUserPath:esPasswordPath:rest) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    user <- T.readFile esUserPath
    pw <- T.readFile esPasswordPath
    --logCxt <- initEsLogContext >>= esLoggingContext user pw
    logCxt <- Mex.initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = MainNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtimemd"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    R.runReaderT (run (initBot (Mex.Leverage 3))) config
