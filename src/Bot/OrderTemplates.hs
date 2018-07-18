module Bot.OrderTemplates
    ( prepareOrder
    , longPosStopLoss
    , shortPosStopLoss
    , limitBuy
    , limitSell
    , orderWithId
    , orderWithClientId
    , closePosition
    ) where

import           BasicPrelude hiding (id)
import qualified BitMEX       as Mex
import           BitMEXClient
    ( ContingencyType (..)
    , ExecutionInstruction (..)
    , OrderType (..)
    , Side (..)
    , Symbol (..)
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
                  map
                      (T.intercalate "," .
                       map (T.pack . show))
                      executionType
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
        Nothing

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
        Nothing

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
        Nothing

limitBuy :: Maybe Text -> Double -> Double -> Mex.Order
limitBuy id orderSize bid =
    prepareOrder
        (OrderID Nothing)
        (ClientID id)
        (LinkID Nothing)
        (Just Limit)
        (Just Buy)
        (LimitPx (Just bid))
        (StopPx Nothing)
        (Qty (Just orderSize))
        (Just [ParticipateDoNotInitiate])
        Nothing

limitSell :: Maybe Text -> Double -> Double -> Mex.Order
limitSell id orderSize ask =
    prepareOrder
        (OrderID Nothing)
        (ClientID id)
        (LinkID Nothing)
        (Just Limit)
        (Just Sell)
        (LimitPx (Just ask))
        (StopPx Nothing)
        (Qty (Just orderSize))
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

orderWithClientId :: ClientID -> Mex.Order
orderWithClientId oid =
    prepareOrder
        (OrderID Nothing)
        oid
        (LinkID Nothing)
        Nothing
        Nothing
        (LimitPx Nothing)
        (StopPx Nothing)
        (Qty Nothing)
        Nothing
        Nothing
