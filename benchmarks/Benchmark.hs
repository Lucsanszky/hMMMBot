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
    , connect
    , getMessage
    , makeRequest
    , makeTimestamp
    , sendMessage
    , sign
    )
import           Bot.Concurrent
    ( processResponse
    )
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
import           Data.IORef                     (newIORef)
import           Data.Monoid
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment
    ( getArgs
    , withArgs
    )

botBenchmark = do
    botState <- R.ask
    config <- BitMEXBot $ lift $ R.ask
    liftIO $ do
      withArgs [] $
          defaultMainWith (defaultConfig { resamples = 1000 })
              [ bench "post orders" $
                nfIO $ unWrapBotWith (placeBulkOrder [limitBuy Nothing 6 666] 6 666 666) botState config

              ]

initBot :: Mex.Leverage -> BitMEXApp ()
initBot lev conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
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
    let botState =
            BotState
            { connection = conn
            , riskManagerQueue =
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
            }
    R.runReaderT (runBot botBenchmark) botState

main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:rest) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    logCxt <- Mex.initLogContext
    let config =
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
    connect
        config
        (initBot
             (Mex.Leverage 3
                  ))
