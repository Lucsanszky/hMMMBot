module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , placeOrder
    , amendOrder
    , initStopLossOrders
    , insertStopLossOrder
    , bulkAmendOrders
    , unWrapBotWith
    ) where

import           BasicPrelude
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
    , Side (..)
    , Symbol (..)
    , makeRequest
    )
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
import qualified Data.HashMap.Strict         as HM (insert)
import qualified Data.Text                   as T (pack)

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
