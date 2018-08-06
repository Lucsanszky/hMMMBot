module Bot
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
    , asks
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
        pnl <- async $ forever $ pnlTracker botState config
        loss <-
            async $
            forever $ lossLimitUpdater botState config
        mapM_ A.link [risk, slw, pnl, loss]
    loop
  where
    loop = loop

initBot :: Mex.Leverage -> BitMEXApp ()
initBot leverage conn = do
    config@BitMEXWrapperConfig {..} <- R.ask
    -- pub <- R.asks publicKey
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
            , leverage = leverage
            }
        base = (drop 8 . show) environment
        path =
            case pathWS of
                Nothing -> "/realtime"
                Just x  -> x
    _ <- updateLeverage XBTUSD leverage
    liftIO $ do
        _ <-
            async $
            withSocketsDo $
            runSecureClient base 443 (LBC.unpack path) $ \c -> do
                processor <-
                    async $
                    forever $ do
                        msg <- getMessage c config
                        processResponse msg botState config
                _ <-
                    async $
                    forever $ do
                        eres <- waitCatch processor
                        case eres of
                            Right _ -> return ()
                            Left _ ->
                                connect
                                    config
                                    (initBot leverage)
                sendMessage
                    c
                    AuthKey
                    [ String publicKey
                    , toJSON time
                    , (toJSON . show) sig
                    ]
                sendMessage
                    c
                    Subscribe
                    ([OrderBook10 XBTUSD] :: [Topic Symbol])
        _ <-
            async $
            withSocketsDo $
            runSecureClient base 443 (LBC.unpack path) $ \c -> do
                processor <-
                    async $
                    forever $ do
                        msg <- getMessage c config
                        processResponse msg botState config
                _ <-
                    async $
                    forever $ do
                        eres <- waitCatch processor
                        case eres of
                            Right _ -> return ()
                            Left _ ->
                                connect
                                    config
                                    (initBot leverage)
                sendMessage
                    c
                    AuthKey
                    [ String publicKey
                    , toJSON time
                    , (toJSON . show) sig
                    ]
                sendMessage
                    c
                    Subscribe
                    ([OrderBookL2 XBTUSD] :: [Topic Symbol])
        _ <-
            async $
            withSocketsDo $
            runSecureClient base 443 (LBC.unpack path) $ \c -> do
                processor <-
                    async $
                    forever $ do
                        msg <- getMessage c config
                        processResponse msg botState config
                _ <-
                    async $
                    forever $ do
                        eres <- waitCatch processor
                        case eres of
                            Right _ -> return ()
                            Left _ ->
                                connect
                                    config
                                    (initBot leverage)
                sendMessage
                    c
                    AuthKey
                    [ String publicKey
                    , toJSON time
                    , (toJSON . show) sig
                    ]
                sendMessage
                    c
                    Subscribe
                    ([Execution] :: [Topic Symbol])
        _ <-
            async $
            withSocketsDo $
            runSecureClient base 443 (LBC.unpack path) $ \c -> do
                processor <-
                    async $
                    forever $ do
                        msg <- getMessage c config
                        processResponse msg botState config
                _ <-
                    async $
                    forever $ do
                        eres <- waitCatch processor
                        case eres of
                            Right _ -> return ()
                            Left _ ->
                                connect
                                    config
                                    (initBot leverage)
                sendMessage
                    c
                    AuthKey
                    [ String publicKey
                    , toJSON time
                    , (toJSON . show) sig
                    ]
                sendMessage
                    c
                    Subscribe
                    ([Position] :: [Topic Symbol])
        processor <-
            async $
            forever $ do
                msg <- getMessage conn config
                processResponse msg botState config
        _ <-
            async $
            forever $ do
                eres <- waitCatch processor
                case eres of
                    Right _ -> return ()
                    Left _ ->
                        connect config (initBot leverage)
        sendMessage
            conn
            AuthKey
            [ String publicKey
            , toJSON time
            , (toJSON . show) sig
            ]
        sendMessage
            conn
            Subscribe
            ([Margin] :: [Topic Symbol])
    R.runReaderT (runBot tradeLoop) botState
