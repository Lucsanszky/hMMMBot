module HMMMBot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
    )
import qualified BitMEX                         as Mex
    ( Accept (..)
    , Leverage (..)
    , Margin (..)
    , MimeJSON (..)
    , MimeResult (..)
    , userGetMargin
    )
import           BitMEXClient
    ( BitMEXReader (..)
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
    ( ask
    , lift
    , runReaderT
    )
import           Control.Monad.STM              (atomically)
import           Data.Aeson
    ( Value (String)
    , toJSON
    )
import           Data.ByteString.Char8          (pack)
import qualified Data.ByteString.Lazy.Char8     as LBC
    ( unpack
    )
import           Data.IORef                     (newIORef)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Network.Socket
    ( withSocketsDo
    )
import           Wuss
    ( runSecureClient
    )

tradeLoop :: BitMEXBot ()
tradeLoop = do
    config <- BitMEXBot $ R.lift R.ask
    botState@BotState {..} <- R.ask
    liftIO $ do
        risk <-
            async $ forever $ riskManager botState config
        slw <-
            async $
            forever $ stopLossWatcher botState config
        --pnl <- async $ forever $ pnlTracker botState config
        loss <-
            async $
            forever $ lossLimitUpdater botState config
        mapM_ A.link [risk, slw, loss]
    loop
  where
    loop = loop

initBot :: Mex.Leverage -> BitMEXReader ()
initBot leverage = do
    config@BitMEXWrapperConfig {..} <- R.ask
    -- pub <- R.asks publicKey
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
            , leverage = leverage
            }
        base = (drop 8 . show) environment
        path =
            case pathWS of
                Nothing -> "/realtime"
                Just x  -> x
    _ <- updateLeverage XBTUSD leverage
    subscriptions <-
        liftIO $
        async $
        withConnectAndSubscribe config [OrderBook10 XBTUSD, OrderBookL2 XBTUSD, Execution, Position, Margin] $ \c ->
            forever $ do
                msg <- getMessage c config
                processResponse msg botState config
    _ <-
        liftIO $
        async $
        forever $ do
            eres <- waitCatch subscriptions
            case eres of
                Right _ -> return ()
                Left _ ->
                    R.runReaderT
                        (run (initBot leverage))
                        config
    R.runReaderT (runBot tradeLoop) botState
