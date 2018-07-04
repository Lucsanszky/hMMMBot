module Bot.OrderTemplates
    ( prepareOrder
    , longPosStopLoss
    , shortPosStopLoss
    , limitBuy
    , limitSell
    , orderWithId
    , closePosition
    ) where

import           BasicPrelude
import qualified BitMEX       as Mex
import           BitMEXClient
    ( BitMEXReader (..)
    , BitMEXWrapperConfig
    , ContingencyType (..)
    , ExecutionInstruction (..)
    , OrderType (..)
    , Side (..)
    , Symbol (..)
    , makeRequest
    )
import           Bot.Types
import qualified Data.Text    as T (intercalate, pack)

prepareOrder ::
       OrderID
    -> ClientID
    -> LinkID
    -> Maybe OrderType
    -> Maybe Side
    -> LimitPx
    -> StopPx
    -> Qty
    -> Maybe [ExecutionInstruction]
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
                  map ((T.intercalate ",") . (map (T.pack . show))) executionType
            , Mex.orderContingencyType =
                  map (T.pack . show) contingencyType
            }
    case ordId of
        Nothing  -> order
        Just oid -> order {Mex.orderOrderId = oid}

longPosStopLoss :: Maybe Double -> Mex.Order
longPosStopLoss stopPx =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just Stop)
        (Just Sell)
        (LimitPx Nothing)
        (StopPx stopPx)
        (Qty Nothing)
        (Just [LastPrice, Close])
        (Nothing)

shortPosStopLoss :: Maybe Double -> Mex.Order
shortPosStopLoss stopPx =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just Stop)
        (Just Buy)
        (LimitPx Nothing)
        (StopPx stopPx)
        (Qty Nothing)
        (Just [LastPrice, Close])
        (Nothing)

closePosition :: Side -> Mex.Order
closePosition side =
    prepareOrder
        (OrderID Nothing)
        (ClientID Nothing)
        (LinkID Nothing)
        (Just Market)
        (Just side)
        (LimitPx Nothing)
        (StopPx Nothing)
        (Qty Nothing)
        (Just [Close])
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
        (Just [ParticipateDoNotInitiate])
        Nothing

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
        (Just [ParticipateDoNotInitiate])
        Nothing

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
