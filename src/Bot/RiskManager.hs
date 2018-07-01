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
    ( bulkAmendOrders
    , cancelOrders
    , initStopLossOrders
    , insertStopLossOrder
    , prepareOrder
    , unWrapBotWith
    )
import           Control.Concurrent.STM.TVar
    ( TVar
    , readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R (asks)
import           Control.Monad.STM           (atomically)
import qualified Data.HashMap.Strict         as HM (lookup)
import           Data.Text                   (Text)
import           Data.Vector                 (head, (!?))
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status   as HTTP
    ( Status (..)
    )

-- TODO: investigate whether the price can change while qty remains the same
updatePositionsAndAmend ::
       Text -> Double -> Maybe Double -> BitMEXBot IO ()
updatePositionsAndAmend stopLoss currQty avgCostPrice = do
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    if null slm
        then return ()
        else do
            let (Just (oid, size)) = HM.lookup stopLoss slm
                (stopPx, placeholder) =
                    if stopLoss == "LONG_POSITION_STOP_LOSS"
                        then ( map (+ 1.5) avgCostPrice
                             , stopLossShort slm)
                        else ( map (flip (-) 1.5)
                                   avgCostPrice
                             , stopLossLong slm)
            if currQty == size
                then return ()
                else do
                    let newStopLoss =
                            (orderWithId
                                 (OrderID (Just oid)))
                            { Mex.orderPrice = avgCostPrice
                            , Mex.orderStopPx = stopPx
                            , Mex.orderOrderQty =
                                  Just currQty
                            }
                    insertStopLossOrder
                        (oid, currQty)
                        stopLoss
                    updatePositionSize currQty
                    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                        bulkAmendOrders
                            [newStopLoss, placeholder]
                    let HTTP.Status {statusCode = code} =
                            responseStatus resp
                    if code == 200
                        then return ()
                        else fail "amending failed"
  where
    stopLossLong slm =
        (orderWithId
             (OrderID
                  (map fst $
                   HM.lookup "LONG_POSITION_STOP_LOSS" slm)))
        { Mex.orderPrice = Just 0.5
        , Mex.orderStopPx = Just 1
        , Mex.orderOrderQty = Just 1
        }
    stopLossShort slm =
        (orderWithId
             (OrderID
                  (map fst $
                   HM.lookup "SHORT_POSITION_STOP_LOSS" slm)))
        { Mex.orderPrice = Just 1000000
        , Mex.orderStopPx = Just 99999
        , Mex.orderOrderQty = Just 1
        }

resetStopLoss :: BitMEXBot IO ()
resetStopLoss = do
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    if null slm
        then return ()
        else do
            let stopLossLong =
                    (orderWithId
                         (OrderID
                              (map fst $
                               HM.lookup
                                   "LONG_POSITION_STOP_LOSS"
                                   slm)))
                    { Mex.orderPrice = Just 0.5
                    , Mex.orderStopPx = Just 1
                    , Mex.orderOrderQty = Just 1
                    }
                stopLossShort =
                    (orderWithId
                         (OrderID
                              (map fst $
                               HM.lookup
                                   "SHORT_POSITION_STOP_LOSS"
                                   slm)))
                    { Mex.orderPrice = Just 1000000
                    , Mex.orderStopPx = Just 99999
                    , Mex.orderOrderQty = Just 1
                    }
            updatePositionSize 0
            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                bulkAmendOrders
                    [stopLossLong, stopLossShort]
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then return ()
                else fail "stopLoss reset failed"

manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk currQty Nothing
    | currQty > 0 =
        updatePositionsAndAmend
            "LONG_POSITION_STOP_LOSS"
            currQty
            Nothing
    | currQty < 0 =
        updatePositionsAndAmend
            "SHORT_POSITION_STOP_LOSS"
            (abs currQty)
            Nothing
    | otherwise = resetStopLoss
manageRisk currQty (Just avgCostPrice)
    | currQty > 0
          -- TODO: get rid of (Just avgCostPrice) with better pattern matching
     =
        updatePositionsAndAmend
            "LONG_POSITION_STOP_LOSS"
            currQty
            (Just (roundPrice $ avgCostPrice * 0.9975))
    | currQty < 0
          -- TODO: get rid of (Just avgCostPrice) with better pattern matching
     =
        updatePositionsAndAmend
            "SHORT_POSITION_STOP_LOSS"
            (abs currQty)
            (Just (roundPrice $ avgCostPrice * 1.0025))
    | otherwise = resetStopLoss

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
                    unWrapBotWith
                        (manageRisk q avgPrice)
                        botState
                        config
        _ -> do
            return ()

setStopLoss :: TVar Bool -> IO ()
setStopLoss t = atomically $ (writeTVar t True)

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
    R.asks stopLossMap >>= \m ->
        liftIO $ atomically $ writeTVar m mempty
    (BitMEXBot . lift $
     makeRequest
         (Mex.orderCancelAll
              (Mex.ContentType Mex.MimeJSON)
              (Mex.Accept Mex.MimeJSON))) >>
        initStopLossOrders

updatePositionSize :: Double -> BitMEXBot IO ()
updatePositionSize x =
    R.asks positionSize >>= \p ->
        (liftIO . atomically) $ writeTVar p (floor x)
