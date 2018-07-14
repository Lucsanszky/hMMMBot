module Bot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
    , last
    )
import qualified BitMEX                         as Mex
import           BitMEXClient
import           Bot.Concurrent
import           Bot.Math
import           Bot.OrderTemplates
import           Bot.RiskManager
import           Bot.Types
import           Bot.Util
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Async       (async)
import qualified Control.Concurrent.Async       as A (link)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Monad.Reader           as R
    ( ask
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
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector
    ( head
    , last
    , (!)
    , (!?)
    )
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

-- TODO: figure out how to do this inside async,
resetOrder ::
       BotState
    -> BitMEXWrapperConfig
    -> Text
    -> Integer
    -> Double
    -> IO ()
resetOrder botState config "Buy" orderSize price = do
    unWrapBotWith
        (cancelLimitOrders "Buy" >>
         placeBulkOrder
             [limitBuy (fromIntegral orderSize) price])
        botState
        config
resetOrder botState config "Sell" orderSize price = do
    unWrapBotWith
        (cancelLimitOrders "Sell" >>
         placeBulkOrder
             [limitSell (fromIntegral orderSize) price])
        botState
        config

trader ::
       BotState
    -> BitMEXWrapperConfig
    -> (Double, Double)
    -> IO ()
trader botState@BotState {..} config (newBestAsk, newBestBid) = do
    when (newBestAsk /= 0 && newBestBid /= 0) $ do
        buyQty <- atomically $ readTVar openBuys
        buyCost <- atomically $ readTVar openBuyCost
        sellQty <- atomically $ readTVar openSells
        sellCost <- atomically $ readTVar openSellCost
        when (buyQty /= 0 && buyCost /= 0) $ do
            let buyAvg =
                    (fromIntegral buyQty) /
                    convert
                        XBt_to_XBT
                        (fromIntegral buyCost)
            when ((abs buyAvg) < newBestBid - 0.25) $ do
                resetOrder
                    botState
                    config
                    "Buy"
                    buyQty
                    newBestBid
                return ()
        when (sellQty /= 0 && sellCost /= 0) $ do
            let sellAvg =
                    (fromIntegral sellQty) /
                    convert
                        XBt_to_XBT
                        (fromIntegral sellCost)
            when ((abs sellAvg) > newBestAsk + 0.25) $ do
                resetOrder
                    botState
                    config
                    "Sell"
                    sellQty
                    newBestAsk
                return ()
        total <- atomically $ readTVar walletBalance
        let orderSize =
                getOrderSize newBestAsk $
                fromIntegral total * lev
            lev = Mex.unLeverage leverage
            limit =
                getLimit newBestAsk $
                fromIntegral total * lev
        available <-
            liftIO $ atomically $ readTVar availableBalance
        if (convert XBt_to_XBT (fromIntegral available)) >
           convert USD_to_XBT newBestAsk *
           (fromIntegral orderSize) /
           lev
            then do
                unWrapBotWith
                    (makeMarket
                         limit
                         orderSize
                         newBestAsk
                         newBestBid)
                    botState
                    config
            else unWrapBotWith
                     (kill "not enough funds")
                     botState
                     config

tradeLoop :: BitMEXBot ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
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

processResponse ::
       BotState
    -> BitMEXWrapperConfig
    -> Maybe Response
    -> IO ()
processResponse botState@BotState {..} config msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 (TABLE {_data = orderBookData}) -> do
                    let RespOrderBook10 { asks = newAsks
                                        , bids = newBids
                                        } =
                            head orderBookData
                        newBestAsk = head $ head newAsks
                        newBestBid = head $ head newBids
                    trader
                        botState
                        config
                        (newBestAsk, newBestBid)
                posResp@(P (TABLE {_data = positionData})) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderBuyCost = buyCost
                                     , openOrderSellQty = sellQty
                                     , openOrderSellCost = sellCost
                                     } = head positionData
                    when (currQty /= Nothing) $ do
                        let Just q = map floor currQty
                        atomically $
                            updateVar positionSize q
                        atomically $
                            writeTBQueue
                                (unRiskManagerQueue
                                     riskManagerQueue)
                                (Just posResp)
                    when (buyQty /= Nothing) $ do
                        let Just b = buyQty
                        atomically $ updateVar openBuys b
                    when (buyCost /= Nothing) $ do
                        let Just bc = buyCost
                        atomically $
                            updateVar openBuyCost bc
                    when (sellQty /= Nothing) $ do
                        let Just s = sellQty
                        atomically $ updateVar openSells s
                    when (sellCost /= Nothing) $ do
                        let Just sc = sellCost
                        atomically $
                            updateVar openSellCost sc
                marginResp@(M (TABLE {_data = marginData})) -> do
                    let RespMargin { realisedPnl = rpnl
                                   , availableMargin = ab
                                   , walletBalance = wb
                                   } = head marginData
                    when (rpnl /= Nothing) $ do
                        let Just p = rpnl
                        atomically $ writeTVar realPnl p
                    when (ab /= Nothing) $ do
                        let Just b = ab
                        atomically $
                            writeTVar availableBalance b
                    when (wb /= Nothing) $ do
                        let Just w = wb
                        atomically $
                            writeTVar walletBalance w
                execResp@(Exe (TABLE {_data = execData})) -> do
                    case execData !? 0 of
                        Nothing -> return ()
                        Just (RespExecution { triggered = text
                                            , ordStatus = stat
                                            }) -> do
                            when
                                (text ==
                                 Just "StopOrderTriggered") $ do
                                atomically $
                                    writeTBQueue
                                        (unSLWQueue slwQueue)
                                        (Just execResp)
                _ -> return ()

initBot :: Mex.Leverage -> BitMEXApp ()
initBot leverage conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    Mex.MimeResult {Mex.mimeResult = res} <-
        makeRequest $
        Mex.userGetMargin (Mex.Accept Mex.MimeJSON)
    let Right (Mex.Margin { Mex.marginWalletBalance = Just wb
                          , Mex.marginAvailableMargin = Just ab
                          }) = res
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 100
    slwQueue <- liftIO $ atomically $ newTBQueue 100
    pnlQueue <- liftIO $ atomically $ newTBQueue 100
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ atomically $ newTVar 0
    realPnl <- liftIO $ atomically $ newTVar 0
    prevBalance <- liftIO $ atomically $ newTVar $ floor wb
    availableBalance <-
        liftIO $ atomically $ newTVar $ floor ab
    walletBalance <-
        liftIO $ atomically $ newTVar $ floor wb
    openBuys <- liftIO $ atomically $ newTVar 0
    openBuyCost <- liftIO $ atomically $ newTVar 0
    openSells <- liftIO $ atomically $ newTVar 0
    openSellCost <- liftIO $ atomically $ newTVar 0
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
            , openBuys = openBuys
            , openBuyCost = openBuyCost
            , openSells = openSells
            , openSellCost = openSellCost
            , stopOrderId = stopOrderId
            , leverage = leverage
            }
    _ <- updateLeverage XBTUSD leverage
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
             , Position
             , Margin
             ] :: [Topic Symbol])
        async $
            forever $ do
                msg <- getMessage conn config
                processResponse botState config msg
    R.runReaderT (runBot tradeLoop) botState
