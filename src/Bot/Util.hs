module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , placeOrder
    , amendOrder
    , initStopLossOrders
    , bulkAmendOrders
    , positionTracker
    , riskManager
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
import           Data.Vector                 (head)
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
                  fmap (T.pack . show) orderType
            , Mex.orderSide = fmap (T.pack . show) side
            , Mex.orderPrice = price
            , Mex.orderStopPx = stopPx
            , Mex.orderOrderQty = orderQty
            , Mex.orderExecInst =
                  fmap (T.pack . show) executionType
            , Mex.orderContingencyType =
                  fmap (T.pack . show) contingencyType
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
    print orders
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
updatePositionsAndAmend stopLoss execQty avgCostPrice = do
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    let (Just (oid, size)) = HM.lookup stopLoss slm
        (side, f, placeholder) =
            if stopLoss == "LONG_POSITION_STOP_LOSS"
                then (Sell, (+ 1), stopLossShort slm)
                else (Buy, (flip (-) 1), stopLossLong slm)
    if execQty == size
        then return ()
        else do
            let newStopLoss =
                    prepareOrder
                        (Just oid)
                        Nothing
                        Nothing
                        Nothing
                        (Just side)
                        avgCostPrice
                        (fmap f avgCostPrice)
                        (Just execQty)
                        Nothing
                        Nothing
            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                bulkAmendOrders [newStopLoss, placeholder]
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then insertStopLossOrder
                         (oid, execQty)
                         stopLoss
                else fail "amending failed"
  where
    stopLossLong slm =
        prepareOrder
            (fmap
                 fst
                 (HM.lookup "LONG_POSITION_STOP_LOSS" slm))
            Nothing
            Nothing
            (Just StopLimit)
            (Just Sell)
            (Just 0.5)
            (Just 1)
            (Just 1)
            (Just LastPrice)
            Nothing
    stopLossShort slm =
        prepareOrder
            (fmap
                 fst
                 (HM.lookup "SHORT_POSITION_STOP_LOSS" slm))
            Nothing
            Nothing
            (Just StopLimit)
            (Just Buy)
            (Just 1000000)
            (Just 99999)
            (Just 1)
            (Just LastPrice)
            Nothing

manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk execQty Nothing
    | execQty > 0 = do
        updatePositionsAndAmend
            "LONG_POSITION_STOP_LOSS"
            execQty
            Nothing
    | execQty < 0 = do
        updatePositionsAndAmend
            "SHORT_POSITION_STOP_LOSS"
            execQty
            Nothing
    | otherwise = return ()
manageRisk _ Nothing = return ()
manageRisk execQty (Just avgCostPrice)
    | execQty > 0
          -- TODO: get rid of (Just avgCostPrice) with better pattern matching
     = do
        updatePositionsAndAmend
            "LONG_POSITION_STOP_LOSS"
            execQty
            (Just (roundPrice $ avgCostPrice * 0.95))
    | execQty < 0
          -- TODO: get rid of (Just avgCostPrice) with better pattern matching
     = do
        updatePositionsAndAmend
            "SHORT_POSITION_STOP_LOSS"
            execQty
            (Just (roundPrice $ avgCostPrice * 1.05))
    | otherwise = return ()

riskManager :: BotState -> BitMEXWrapperConfig -> IO ()
riskManager botState@BotState {..} config = do
    resp <- atomically $ readResponse positionQueue
    case resp of
        P (TABLE {_data = positionData}) -> do
            let RespPosition { execQty = qty
                             , avgCostPrice = avgPrice
                             } = head positionData
            case qty of
                Nothing -> return ()
                Just q ->
                    unWrapBotWith
                        (manageRisk q avgPrice)
                        botState
                        config
        _ -> return ()

-------------------------------------------------------------
-- POSITION TRACKER
-------------------------------------------------------------
updatePositionSize :: Double -> BitMEXBot IO ()
updatePositionSize x =
    R.asks positionSize >>= \p ->
        (liftIO . atomically) $ writeTVar p (floor x)

positionTracker :: BotState -> BitMEXWrapperConfig -> IO ()
positionTracker botState@BotState {..} config = do
    resp <- atomically $ readResponse positionQueue
    case resp of
        P (TABLE {_data = positionData}) -> do
            let RespPosition {currentQty = qty} =
                    head positionData
            case qty of
                Nothing -> return ()
                Just q -> do
                    unWrapBotWith
                        (updatePositionSize q)
                        botState
                        config
        _ -> return ()

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
