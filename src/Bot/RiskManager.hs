module Bot.RiskManager
    ( riskManager
    , stopLossWatcher
    ) where

import           BasicPrelude                hiding (head)
import qualified BitMEX                      as Mex
    ( Accept (..)
    , ContentType (..)
    , MimeJSON (..)
    , MimeResult (..)
    , orderCancelAll
    )
import           BitMEXClient
    ( BitMEXWrapperConfig
    , RespExecution (..)
    , RespPosition (..)
    , Response (..)
    , Side (..)
    , TABLE (..)
    , makeRequest
    )
import           Bot.Concurrent              (readResponse)
import           Bot.Math                    (roundPrice)
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    )
import           Bot.Util
    ( bulkAmendOrders
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
    triggered <- R.asks stopLossTriggered >>= (liftIO . atomically . readTVar)
    if null slm
        then return ()
        else do
            let (Just (oid, size)) = HM.lookup stopLoss slm
                (f, placeholder) =
                    if stopLoss == "LONG_POSITION_STOP_LOSS"
                        then ((+ 1.5), stopLossShort slm)
                        else ( (flip (-) 1.5)
                             , stopLossLong slm)
            if currQty == size
                then return ()
                else do
                    let newStopLoss =
                            prepareOrder
                                (Just oid)
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                avgCostPrice
                                (if triggered
                                    then Nothing
                                    else (map f avgCostPrice))
                                (Just currQty)
                                Nothing
                                Nothing
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
        prepareOrder
            (map fst
                 (HM.lookup "LONG_POSITION_STOP_LOSS" slm))
            Nothing
            Nothing
            Nothing
            Nothing
            (Just 0.5)
            (Just 1)
            (Just 1)
            Nothing
            Nothing
    stopLossShort slm =
        prepareOrder
            (map fst
                 (HM.lookup "SHORT_POSITION_STOP_LOSS" slm))
            Nothing
            Nothing
            Nothing
            (Just Buy)
            (Just 1000000)
            (Just 99999)
            (Just 1)
            Nothing
            Nothing

resetStopLoss :: BitMEXBot IO ()
resetStopLoss = do
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    if null slm
        then return ()
        else do
            let stopLossLong =
                    prepareOrder
                        (map fst
                             (HM.lookup
                                  "LONG_POSITION_STOP_LOSS"
                                  slm))
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        (Just 0.5)
                        (Just 1)
                        (Just 1)
                        Nothing
                        Nothing
                stopLossShort =
                    prepareOrder
                        (map fst
                             (HM.lookup
                                 "SHORT_POSITION_STOP_LOSS"
                                  slm))
                        Nothing
                        Nothing
                        Nothing
                        (Just Buy)
                        (Just 1000000)
                        (Just 99999)
                        (Just 1)
                        Nothing
                        Nothing
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
    resp <- atomically $ readResponse positionQueue
    case resp of
        P (TABLE {_data = positionData}) -> do
            let RespPosition { execQty = qty
                             , currentQty = currQty
                             , avgCostPrice = avgPrice
                             } = head positionData
            -- asshole way to check if both values are "Just"
            -- while keeping the second value
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

toggleStopLoss :: TVar Bool -> IO ()
toggleStopLoss t = atomically $ (writeTVar t True)

stopLossWatcher :: BotState -> BitMEXWrapperConfig -> IO ()
stopLossWatcher botState@BotState {..} config = do
    resp <- atomically $ readResponse executionQueue
    case resp of
        Exe (TABLE {_data = execData}) -> do
            case execData !? 0 of
                Nothing -> return ()
                Just (RespExecution { triggered = text
                                    , ordStatus = stat
                                    }) ->
                    case text of
                        Just "StopOrderTriggered" -> case stat of
                            Just "Filled" -> unWrapBotWith restart botState config
                            _ -> toggleStopLoss stopLossTriggered
                        _ -> return ()
        _ -> return ()

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
