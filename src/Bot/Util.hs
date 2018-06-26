module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , placeOrder
    , amendOrder
    , initStopLossOrders
    , bulkAmendOrders
    , riskManager
    , stopLossWatcher
    , unWrapBotWith
    ) where

import           BasicPrelude                hiding (head)
import qualified BitMEX                      as Mex
    ( Accept (..)
    , BitMEXRequest (..)
    , ContentType (..)
    , MimeError (..)
    , MimeJSON (..)
    , MimeResult (..)
    , Order (..)
    , Symbol (..)
    , mkOrder
    , orderAmend
    , orderAmendBulk
    , orderCancelAll
    , orderNew
    , orderNewBulk
    , _setBodyLBS
    )
import           BitMEXClient
    ( BitMEXReader (..)
    , BitMEXWrapperConfig
    , ContingencyType
    , ExecutionInstruction (..)
    , OrderType
    , OrderType (..)
    , RespOrder (..)
    , RespPosition (..)
    , Response (..)
    , Side (..)
    , Symbol (..)
    , TABLE (..)
    , makeRequest
    )
import           Bot.Concurrent              (readResponse)
import           Bot.Math                    (roundPrice)
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    )
import           Control.Concurrent.STM.TVar
    ( readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R
    ( asks
    , runReaderT
    )
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (encode)
import qualified Data.HashMap.Strict         as HM
    ( insert
    , lookup
    )
import qualified Data.Text                   as T (pack)
import           Data.Vector                 (head, (!?))
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status   as HTTP
    ( Status (..)
    )

-------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------
unWrapBotWith ::
       (BitMEXBot IO ())
    -> BotState
    -> BitMEXWrapperConfig
    -> IO ()
unWrapBotWith f botState config =
    R.runReaderT
        (run (R.runReaderT (runBot f) botState))
        config

-------------------------------------------------------------
-- ORDERS
-------------------------------------------------------------
prepareOrder ::
       Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe OrderType
    -> Maybe Side
    -> Maybe Double
    -> Maybe Double
    -> Maybe Double
    -> Maybe ExecutionInstruction
    -> Maybe ContingencyType
    -> Mex.Order
prepareOrder ordId clientId linkId orderType side price stopPx orderQty executionType contingencyType = do
    let order =
            (Mex.mkOrder "")
            { Mex.orderSymbol =
                  Just ((T.pack . show) XBTUSD)
            , Mex.orderClOrdId = clientId
            , Mex.orderClOrdLinkId = linkId
            , Mex.orderOrdType =
                  map (T.pack . show) orderType
            , Mex.orderSide = map (T.pack . show) side
            , Mex.orderPrice = price
            , Mex.orderStopPx = stopPx
            , Mex.orderOrderQty = orderQty
            , Mex.orderExecInst =
                  map (T.pack . show) executionType
            , Mex.orderContingencyType =
                  map (T.pack . show) contingencyType
            }
    case ordId of
        Nothing  -> order
        Just oid -> order {Mex.orderOrderId = oid}

placeOrder ::
       Mex.Order -> BitMEXBot IO (Mex.MimeResult Mex.Order)
placeOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNew
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) XBTUSD))
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    BitMEXBot . lift $ makeRequest orderRequest

placeBulkOrder ::
       [Mex.Order]
    -> BitMEXBot IO (Mex.MimeResult [Mex.Order])
placeBulkOrder orders = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNewBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orders\": " <> encode orders <> "}"
    BitMEXBot . lift $ makeRequest orderRequest

amendOrder ::
       Mex.Order -> BitMEXBot IO (Mex.MimeResult Mex.Order)
amendOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderAmend
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    BitMEXBot . lift $ makeRequest orderRequest

bulkAmendOrders ::
       [Mex.Order]
    -> BitMEXBot IO (Mex.MimeResult [Mex.Order])
bulkAmendOrders orders = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderAmendBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orders\": " <> encode orders <> "}"
    BitMEXBot . lift $ makeRequest orderRequest

insertStopLossOrder ::
       (Text, Double) -> Text -> BitMEXBot IO ()
insertStopLossOrder (oid, qty) stopLossType = do
    stopMap <- R.asks stopLossMap
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    liftIO $
        atomically $
        writeTVar stopMap $
        HM.insert stopLossType (oid, qty) slm

initStopLossOrders :: BitMEXBot IO ()
initStopLossOrders = do
    let stopLossBuy =
            prepareOrder
                Nothing
                Nothing
                Nothing
                (Just StopLimit)
                (Just Sell)
                (Just 0.5)
                (Just 1)
                (Just 1)
                (Just LastPrice)
                Nothing
        stopLossSell =
            prepareOrder
                Nothing
                Nothing
                Nothing
                (Just StopLimit)
                (Just Buy)
                (Just 1000000)
                (Just 99999)
                (Just 1)
                (Just LastPrice)
                Nothing
    Mex.MimeResult {mimeResult = res} <-
        placeBulkOrder [stopLossBuy, stopLossSell]
    case res of
        Left (Mex.MimeError {mimeError = s}) -> fail s
        Right orders ->
            mapM_
                (\Mex.Order { orderOrderId = oid
                            , orderSide = oside
                            , orderOrderQty = Just qty
                            } ->
                     case oside of
                         Nothing ->
                             fail
                                 "CRITICAL: order side is missing"
                         Just s ->
                             if s == "Buy"
                                 then insertStopLossOrder
                                          (oid, qty)
                                          "SHORT_POSITION_STOP_LOSS"
                                 else insertStopLossOrder
                                          (oid, qty)
                                          "LONG_POSITION_STOP_LOSS")
                orders

-------------------------------------------------------------
-- RISK MANAGER
-------------------------------------------------------------
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
                (side, f, placeholder) =
                    if stopLoss == "LONG_POSITION_STOP_LOSS"
                        then ( Sell
                             , (+ 0.5)
                             , stopLossShort slm)
                        else ( Buy
                             , (flip (-) 0.5)
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
                                (map f avgCostPrice)
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

stopLossWatcher :: BotState -> BitMEXWrapperConfig -> IO ()
stopLossWatcher botState@BotState {..} config = do
    resp <- atomically $ readResponse orderQueue
    case resp of
        O (TABLE {_data = orderData}) -> do
            case orderData !? 0 of
                Nothing -> return ()
                Just (RespOrder {triggered = text}) ->
                    case text of
                        Nothing -> return ()
                        Just t -> do
                            if t == "StopOrderTriggered"
                                then unWrapBotWith
                                         restart
                                         botState
                                         config
                                else return ()
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

-------------------------------------------------------------
-- POSITION TRACKER
-------------------------------------------------------------
updatePositionSize :: Double -> BitMEXBot IO ()
updatePositionSize x =
    R.asks positionSize >>= \p ->
        (liftIO . atomically) $ writeTVar p (floor x)

-------------------------------------------------------------
-- MARKET MAKING
-------------------------------------------------------------
makeMarket ::
       Double
    -> Double
    -> BitMEXBot IO (Mex.MimeResult [Mex.Order])
makeMarket ask bid = do
    let buyOrder =
            prepareOrder
                Nothing
                Nothing
                Nothing
                (Just Limit)
                (Just Buy)
                (Just bid)
                Nothing
                (Just 21)
                Nothing
                Nothing
        sellOrder =
            prepareOrder
                Nothing
                Nothing
                Nothing
                (Just Limit)
                (Just Sell)
                (Just ask)
                Nothing
                (Just 21)
                Nothing
                Nothing
    placeBulkOrder [buyOrder, sellOrder]
