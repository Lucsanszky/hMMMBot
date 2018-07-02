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
    , orderCancelAll
    )
import           BitMEXClient
    ( BitMEXWrapperConfig
    , RespExecution (..)
    , RespMargin (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    , makeRequest
    )
import           Bot.Concurrent              (readResponse)
import           Bot.Math                    (roundPrice)
import           Bot.OrderTemplates
import           Bot.Types
import           Bot.Util
    ( cancelOrders
    , cancelStopOrder
    , placeOrder
    , placeStopOrder
    , unWrapBotWith
    )
import           Control.Concurrent.STM.TVar
    ( TVar
    , readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R (asks)
import           Control.Monad.STM           (atomically)
import           Data.Vector                 (head, (!?))

manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk 0 _ = do
    OrderID oid <-
        R.asks stopOrderId >>=
        (liftIO . atomically . readTVar)
    case oid of
        Just i -> do
            cancelOrders [i] >> R.asks stopOrderId >>= \o ->
                liftIO $
                atomically $ writeTVar o (OrderID Nothing)
            pSize <- R.asks prevPosition
            (liftIO $ atomically $ writeTVar pSize (floor 0))
        Nothing -> return ()
manageRisk _ Nothing = return ()
manageRisk currQty avgCostPrice
    | currQty > 0 = do
        OrderID oid <-
            R.asks stopOrderId >>=
            (liftIO . atomically . readTVar)
        let roundedPrice =
                map (roundPrice . (* 0.9925)) avgCostPrice
            newStopLoss = longPosStopLoss roundedPrice
        case oid of
            Nothing -> do
                placeStopOrder (placeOrder newStopLoss)
                pSize <- R.asks prevPosition
                liftIO $
                    atomically $
                    writeTVar pSize (floor currQty)
            Just _ -> do
                currPos <-
                    R.asks prevPosition >>=
                    (liftIO . atomically . readTVar)
                if currPos < 0
                    then do
                        cancelStopOrder (OrderID oid)
                        placeStopOrder
                            (placeOrder newStopLoss)
                        pSize <- R.asks prevPosition
                        liftIO $
                            atomically $
                            writeTVar pSize (floor currQty)
                    else return ()
    | currQty < 0 = do
        OrderID oid <-
            R.asks stopOrderId >>=
            (liftIO . atomically . readTVar)
        let roundedPrice =
                map (roundPrice . (* 1.0075)) avgCostPrice
            newStopLoss = shortPosStopLoss roundedPrice
        case oid of
            Nothing -> do
                placeStopOrder (placeOrder newStopLoss)
                pSize <- R.asks prevPosition
                liftIO $
                    atomically $
                    writeTVar pSize (floor currQty)
            Just _ -> do
                currPos <-
                    R.asks prevPosition >>=
                    (liftIO . atomically . readTVar)
                if currPos > 0
                    then do
                        cancelStopOrder (OrderID oid)
                        placeStopOrder
                            (placeOrder newStopLoss)
                        pSize <- R.asks prevPosition
                        liftIO $
                            atomically $
                            writeTVar pSize (floor currQty)
                    else do
                        return ()

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
                Just (RespExecution { triggered = text
                                    , ordStatus = stat
                                    , side = s
                                    }) ->
                    case text of
                        Just "StopOrderTriggered" ->
                            unWrapBotWith
                                restart
                                botState
                                config
                        _ -> return ()
        _ -> return ()

pnlTracker :: PnLQueue -> IO ()
pnlTracker q =
    (atomically $ readResponse $ unPnlQueue q) >>= \(M (TABLE {_data = marginData})) -> do
        let RespMargin {realisedPnl = rpnl} =
                head marginData
        print rpnl

restart :: BitMEXBot IO ()
restart
 = do
    (R.asks stopOrderId >>= \m ->
         liftIO $ atomically $ writeTVar m (OrderID Nothing)) >>
        (BitMEXBot . lift $
         makeRequest
             (Mex.orderCancelAll
                  (Mex.ContentType Mex.MimeJSON)
                  (Mex.Accept Mex.MimeJSON))) >>
        return ()

-- update :: TVar Int -> Double -> IO ()
-- update v x = (liftIO . atomically) $ writeTVar v (floor x)
