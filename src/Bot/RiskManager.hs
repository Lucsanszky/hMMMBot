module Bot.RiskManager
    ( riskManager
    , stopLossWatcher
    , pnlTracker
    ) where

import           BasicPrelude                hiding (head)
import qualified BasicPrelude                as BP (head)
import qualified BitMEX                      as Mex
    ( Accept (..)
    , ContentType (..)
    , MimeError (..)
    , MimeJSON (..)
    , MimeResult (..)
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
    ( amendOrder
    , bulkAmendOrders
    , cancelOrders
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
import qualified Data.HashMap.Strict         as HM
    ( lookup
    , toList
    )
import           Data.Text                   (Text)
import           Data.Vector                 (head, (!?))
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status   as HTTP
    ( Status (..)
    )

manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk currQty avgCostPrice
    | currQty > 0 = do
        OrderID oid <-
            R.asks stopOrderId >>=
            (liftIO . atomically . readTVar)
        let roundedPrice =
                map (roundPrice . (* 0.9970)) avgCostPrice
            stopPx = (map (+ 1.5) roundedPrice)
            newStopLoss = longPosStopLoss stopPx
        case oid of
            Nothing -> do
                placeStopOrder (placeOrder newStopLoss)
            Just _ -> do
                currPos <-
                    R.asks positionSize >>=
                    (liftIO . atomically . readTVar)
                if currPos < 0
                    then do
                        cancelStopOrder (OrderID oid)
                        placeStopOrder
                            (placeOrder newStopLoss)
                    else return ()
    | currQty < 0 = do
        OrderID oid <-
            R.asks stopOrderId >>=
            (liftIO . atomically . readTVar)
        let roundedPrice =
                map (roundPrice . (* 1.0030)) avgCostPrice
            stopPx = (map (flip (-) 1.5) roundedPrice)
            newStopLoss = shortPosStopLoss stopPx
        case oid of
            Nothing -> do
                placeStopOrder (placeOrder newStopLoss)
            Just _ -> do
                currPos <-
                    R.asks positionSize >>=
                    (liftIO . atomically . readTVar)
                if currPos > 0
                    then do
                        cancelStopOrder (OrderID oid)
                        placeStopOrder
                            (placeOrder newStopLoss)
                    else return ()
    | otherwise = do
        OrderID oid <-
            R.asks stopOrderId >>=
            (liftIO . atomically . readTVar)
        case oid of
            Just i ->
                cancelOrders [i] >> R.asks stopOrderId >>= \o ->
                    liftIO $
                    atomically $
                    writeTVar o (OrderID Nothing)
            Nothing -> return ()
manageRisk _ Nothing = return ()

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
            -- asshole way to check if both values are "Just"
            -- while keeping the second value
            -- TODO: simplify, since we can
            case qty >> currQty of
                Nothing -> do
                    case currQty of
                        Nothing -> return ()
                        Just cq ->
                            unWrapBotWith
                                (updatePositionSize cq)
                                botState
                                config
                Just q -> do
                    case avgPrice of
                        Just _ ->
                            unWrapBotWith
                                (manageRisk q avgPrice >>
                                 updatePositionSize q)
                                botState
                                config
                        Nothing ->
                            unWrapBotWith
                                (updatePositionSize q)
                                botState
                                config
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
    -- Immediately empty the stopLossMap
    -- so other threads won't attempt to make orders
 = do
    (R.asks stopOrderId >>= \m ->
         liftIO $ atomically $ writeTVar m (OrderID Nothing)) >>
        (BitMEXBot . lift $
         makeRequest
             (Mex.orderCancelAll
                  (Mex.ContentType Mex.MimeJSON)
                  (Mex.Accept Mex.MimeJSON))) >>
        return ()

updatePositionSize :: Double -> BitMEXBot IO ()
updatePositionSize x =
    R.asks positionSize >>= \p ->
        (liftIO . atomically) $ writeTVar p (floor x)
