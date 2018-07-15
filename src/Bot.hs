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
import           Data.IORef
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
              (placeBulkOrder
                   [limitBuy (fromIntegral orderSize) price]
                   orderSize
                   price
                   price))
            botState
            config
resetOrder botState config "Sell" orderSize price = do
        unWrapBotWith
            (cancelLimitOrders "Sell" >>
              (placeBulkOrder
                   [limitBuy (fromIntegral orderSize) price]
                   orderSize
                   price
                   price))
            botState
            config

trader ::
       BotState
    -> BitMEXWrapperConfig
    -> (Double, Double)
    -> (IORef Double, IORef Double)
    -> IO ()
trader botState@BotState {..} config (newBestAsk, newBestBid) (prevAsk, prevBid) = do
    prevAsk' <- readIORef prevAsk
    prevBid' <- readIORef prevBid
    sellQty <- atomically $ readTVar openSells
    buyQty <- atomically $ readTVar openBuys
    posSize <- readIORef positionSize
    when (posSize == 0 && buyQty == 0 && sellQty == 0) $ do
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
        return ()
    when (newBestAsk /= prevAsk' || newBestBid /= prevBid') $ do
        atomicWriteIORef prevAsk newBestAsk
        atomicWriteIORef prevBid newBestBid
        let diff = newBestAsk - newBestBid
        when (sellQty == 0 && buyQty /= 0) $ do
            if (diff > 0.5)
               then do
                  resetOrder
                      botState
                      config
                      "Buy"
                      (abs posSize)
                      (newBestAsk - 0.5)
                  atomicWriteIORef prevAsk (newBestAsk - 0.5)
               else
                  resetOrder
                      botState
                      config
                      "Buy"
                      (abs posSize)
                      newBestBid
            return ()
	when (buyQty == 0 && sellQty /= 0) $ do
            if (diff > 0.5)
               then do
                  resetOrder
                      botState
                      config
                      "Sell"
                      posSize
                      (newBestBid + 0.5)
                  atomicWriteIORef prevBid (newBestBid + 0.5)
               else
                  resetOrder
                      botState
                      config
                      "Sell"
                      posSize
                      newBestAsk
            return ()

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
    -> (IORef Double, IORef Double)
    -> Maybe Response
    -> IO ()
processResponse botState@BotState {..} config prevPrices msg = do
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
                        prevPrices
                posResp@(P (TABLE {_data = positionData})) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderBuyCost = buyCost
                                     , openOrderSellQty = sellQty
                                     , openOrderSellCost = sellCost
                                     } = head positionData
                    when (currQty /= Nothing) $ do
                        let Just q = map floor currQty
                        atomicWriteIORef positionSize q
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
    positionSize <- liftIO $ newIORef 0
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
    prevBid <- liftIO $ newIORef 0.0
    prevAsk <- liftIO $ newIORef 0.0
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
        processor <-
            async $
            forever $ do
                msg <- getMessage conn config
                processResponse botState config (prevAsk, prevBid) msg
        A.link processor
    R.runReaderT (runBot tradeLoop) botState
