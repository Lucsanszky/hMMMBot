module Bot.RiskManager
    ( riskManager
    , stopLossWatcher
    , pnlTracker
    ) where

import           BasicPrelude                hiding (head)
import qualified BitMEX                      as Mex
    ( Accept (..)
    , ContentType (..)
    , MimeJSON (..)
    , Order (..)
    , orderCancelAll
    )
import           BitMEXClient
    ( BitMEXWrapperConfig
    , RespExecution (..)
    , RespMargin (..)
    , RespPosition (..)
    , Response (..)
    , Side (..)
    , TABLE (..)
    , makeRequest
    )
import           Bot.Concurrent              (readResponse)
import           Bot.Math                    (roundPrice)
import           Bot.OrderTemplates
import           Bot.Types
import           Bot.Util
    ( cancelLimitOrders
    , cancelOrders
    , cancelStopOrder
    , kill
    , placeOrder
    , placeStopOrder
    , restart
    , unWrapBotWith
    )
import           Control.Concurrent.STM.TVar
    ( readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R
    ( ask
    , asks
    )
import           Control.Monad.STM           (atomically)
import           Data.Vector                 (head, (!?))

manageStopLoss ::
       Mex.Order -> PositionType -> BitMEXBot IO ()
manageStopLoss newStopLoss newPos = do
    OrderID oid <-
        R.asks stopOrderId >>=
        (liftIO . atomically . readTVar)
    prevPos <-
        R.asks prevPosition >>=
        (liftIO . atomically . readTVar)
    when (oid == Nothing || prevPos /= newPos) $ do
        when (oid /= Nothing && prevPos /= newPos) $
            cancelStopOrder (OrderID oid)
        placeStopOrder (placeOrder newStopLoss)
        pSize <- R.asks prevPosition
        liftIO $ atomically $ writeTVar pSize newPos
    return ()

manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk 0 _ = do
    OrderID oid <-
        R.asks stopOrderId >>=
        (liftIO . atomically . readTVar)
    case oid of
        Just i -> do
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
        P (TABLE {_data = positionData}) -> do
            let RespPosition { execQty = qty
                             , currentQty = currQty
                             , avgCostPrice = avgPrice
                             } = head positionData
            case qty >> currQty of
                Nothing -> return ()
                Just q -> do
                    (unWrapBotWith
                         (manageRisk q avgPrice)
                         botState
                         config)
        _ -> do
            return ()

stopLossWatcher :: BotState -> BitMEXWrapperConfig -> IO ()
stopLossWatcher botState@BotState {..} config = do
    resp <- atomically $ readResponse $ unSLWQueue slwQueue
    case resp of
        Exe (TABLE {_data = execData}) -> do
            case execData !? 0 of
                Nothing -> return ()
                Just (RespExecution {triggered = text}) ->
                    case text of
                        Just "StopOrderTriggered" ->
                            unWrapBotWith
                                restart
                                botState
                                config
                        _ -> return ()
        _ -> return ()

pnlTracker :: BotState -> BitMEXWrapperConfig -> IO ()
pnlTracker botState@BotState {..} config = do
    prev <- liftIO $ atomically $ readTVar prevBalance
    current <- liftIO $ atomically $ readTVar walletBalance
    if fromIntegral current / fromIntegral prev <= 0.75
        then do
            unWrapBotWith
                (kill "lost too much")
                botState
                config
        else
            return ()
