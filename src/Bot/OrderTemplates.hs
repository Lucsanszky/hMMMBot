module Bot.OrderTemplates
    ( prepareOrder
    , longPosStopLoss
    , shortPosStopLoss
    , limitBuy
    , limitSell
    , orderWithId
    ) where

import           BasicPrelude
import qualified BitMEX       as Mex
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
import qualified Data.Text    as T (pack)

prepareOrder ::
       OrderID
    -> ClientID
    -> LinkID
    -> Maybe OrderType
    -> Maybe Side
    -> LimitPx
    -> StopPx
    -> Qty
    -> Maybe ExecutionInstruction
    -> Maybe ContingencyType
    -> Mex.Order
prepareOrder (OrderID ordId) (ClientID clientId) (LinkID linkId) orderType side (LimitPx price) (StopPx stopPx) (Qty orderQty) executionType contingencyType = do
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

longPosStopLoss :: Mex.Order
longPosStopLoss =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just StopLimit)
        (Just Sell)
        (LimitPx (Just 0.5))
        (StopPx (Just 1))
        (Qty (Just 1))
        (Just LastPrice)
        (Nothing)

shortPosStopLoss :: Mex.Order
shortPosStopLoss =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just StopLimit)
        (Just Buy)
        (LimitPx (Just 1000000))
        (StopPx (Just 999999))
        (Qty (Just 1))
        (Just LastPrice)
        (Nothing)

limitBuy :: Double -> Mex.Order
limitBuy bid =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just Limit)
        (Just Buy)
        (LimitPx (Just bid))
        (StopPx Nothing)
        (Qty (Just 21))
        (Nothing)
        (Nothing)

limitSell :: Double -> Mex.Order
limitSell ask =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just Limit)
        (Just Sell)
        (LimitPx (Just ask))
        (StopPx Nothing)
        (Qty (Just 21))
        (Nothing)
        (Nothing)

orderWithId :: OrderID -> Mex.Order
orderWithId oid =
    prepareOrder
        oid
        (ClientID Nothing)
        (LinkID Nothing)
        Nothing
        Nothing
        (LimitPx Nothing)
        (StopPx Nothing)
        (Qty Nothing)
        Nothing
        Nothing
