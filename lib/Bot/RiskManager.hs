module Bot.RiskManager
    ( riskManager
    , stopLossWatcher
    , pnlTracker
    , lossLimitUpdater
    ) where

import           BasicPrelude                hiding (head)
import qualified BitMEX                      as Mex
    ( Order (..)
    )
import           BitMEXClient
    ( BitMEXWrapperConfig
    , RespExecution (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    )
import           Bot.Concurrent              (readResponse)
import           Bot.Math                    (roundPrice)
import           Bot.OrderTemplates
    ( limitBuy
    , limitSell
    , longPosStopLoss
    , shortPosStopLoss
    )
import           Bot.Types
    ( BitMEXBot
    , BotState (..)
    , OrderID (..)
    , PositionType (..)
    , RiskManagerQueue (..)
    , StopLossWatcherQueue (..)
    )
import           Bot.Util
    ( cancelStopOrder
    , kill
    , placeBulkOrder
    , placeOrder
    , placeStopOrder
    , restart
    , unWrapBotWith
    )
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM.TVar
    ( readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R (asks)
import           Control.Monad.STM           (atomically)
import           Data.IORef                  (readIORef)
import           Data.Vector                 (head, (!?))

manageStopLoss :: Mex.Order -> PositionType -> BitMEXBot ()
manageStopLoss newStopLoss newPos = do
    OrderID oid <-
        R.asks stopOrderId >>=
        (liftIO . atomically . readTVar)
    prevPos <-
        R.asks prevPosition >>=
        (liftIO . atomically . readTVar)
    when (isNothing oid || prevPos /= newPos) $ do
        when (isJust oid && prevPos /= newPos) $
            cancelStopOrder (OrderID oid)
        placeStopOrder (placeOrder newStopLoss)
        pSize <- R.asks prevPosition
        liftIO $ atomically $ writeTVar pSize newPos
    return ()

manageRisk :: Double -> Maybe Double -> BitMEXBot ()
manageRisk 0 _ = do
    OrderID oid <-
        R.asks stopOrderId >>=
        (liftIO . atomically . readTVar)
    case oid of
        Just _ -> do
            cancelStopOrder (OrderID oid)
            pSize <- R.asks prevPosition
            liftIO $ atomically $ writeTVar pSize None
        Nothing -> return ()
manageRisk _ Nothing = return ()
manageRisk currQty avgCostPrice
    | currQty > 0 = do
        let roundedPrice =
                map (roundPrice . (* 0.99)) avgCostPrice
            newStopLoss = longPosStopLoss roundedPrice
        manageStopLoss newStopLoss Long
    | currQty < 0 = do
        let roundedPrice =
                map (roundPrice . (* 1.01)) avgCostPrice
            newStopLoss = shortPosStopLoss roundedPrice
        manageStopLoss newStopLoss Short
    | otherwise = return ()

riskManager :: BotState -> BitMEXWrapperConfig -> IO ()
riskManager botState@BotState {..} config = do
    resp <-
        atomically $
        readResponse $ unRiskManagerQueue riskManagerQueue
    case resp of
        P TABLE {_data = positionData} -> do
            let RespPosition { execQty = qty
                             , currentQty = currQty
                             , avgCostPrice = avgPrice
                             } = head positionData
            case currQty of
                Nothing -> return ()
                Just q -> do
                    sellQty <- readIORef openSells
                    buyQty <- readIORef openBuys
                    when (buyQty == 0 && q < 0) $ do
                        ask <- readIORef bestAsk
                        bid <- readIORef bestBid
                        let o =
                                [ limitBuy
                                      Nothing
                                      (abs q)
                                      bid
                                ]
                        unWrapBotWith
                            (placeBulkOrder
                                 o
                                 ((abs . truncate) q)
                                 ask
                                 bid 5)
                            botState
                            config
                    when (sellQty == 0 && q > 0) $ do
                        ask <- readIORef bestAsk
                        bid <- readIORef bestBid
                        let o = [limitSell Nothing q ask]
                        unWrapBotWith
                            (placeBulkOrder
                                 o
                                 (truncate q)
                                 ask
                                 bid 5)
                            botState
                            config
            case qty >> currQty of
                Nothing -> return ()
                Just q ->
                    unWrapBotWith
                        (manageRisk q avgPrice)
                        botState
                        config
        _ -> return ()

stopLossWatcher :: BotState -> BitMEXWrapperConfig -> IO ()
stopLossWatcher botState@BotState {..} config = do
    resp <- atomically $ readResponse $ unSLWQueue slwQueue
    case resp of
        Exe TABLE {_data = execData} ->
            case execData !? 0 of
                Nothing -> return ()
                Just RespExecution {triggered = text} ->
                    case text of
                        Just "StopOrderTriggered" ->
                            unWrapBotWith
                                restart
                                botState
                                config
                        _ -> return ()
        _ -> return ()

-- | Update the balance every 8 hours
lossLimitUpdater :: BotState -> BitMEXWrapperConfig -> IO ()
lossLimitUpdater BotState {..} _ = do
    threadDelay 28800000000
    current <- liftIO $ atomically $ readTVar walletBalance
    liftIO $ atomically $ writeTVar prevBalance current

pnlTracker :: BotState -> BitMEXWrapperConfig -> IO ()
pnlTracker botState@BotState {..} config = do
    prev <- liftIO $ atomically $ readTVar prevBalance
    current <- liftIO $ atomically $ readTVar walletBalance
    when (fromIntegral current / fromIntegral prev <= 0.9) $
        unWrapBotWith (kill "lost too much") botState config
