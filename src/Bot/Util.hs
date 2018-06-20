{-# LANGUAGE FlexibleContexts #-}

module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , placeOrder
    , amendOrder
    , initStopLossOrders
    , manageRisk
    , bulkAmendOrders
    ) where

import           BasicPrelude
import qualified BitMEX                as Mex
import           BitMEXClient
import           Data.Aeson
    ( Value (String)
    , encode
    , toJSON
    )
import qualified Data.HashMap.Strict   as HM (lookup)
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T (pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)

manageRisk ::
       HashMap Text Text
    -> Double
    -> Maybe Double
    -> BitMEXReader IO ()
manageRisk stopLossMap _ Nothing = return ()
manageRisk stopLossMap cumQty (Just avgCostPrice)
    | cumQty > 0 = do
        time <- liftIO $ makeTimestamp <$> getPOSIXTime
        let stopLossBuy =
                prepareOrder
                    Nothing
                    (fromJust $ HM.lookup "LONG_POSITION_STOP_LOSS" stopLossMap)
                    Nothing
                    (Just Sell)
                    (avgCostPrice * 0.95)
                    (Just (avgCostPrice * 0.95 + 1))
                    cumQty
                    Nothing
                    Nothing
        res <- bulkAmendOrders [stopLossBuy]
        return ()
    | otherwise = do
        time <- liftIO $ makeTimestamp <$> getPOSIXTime
        let stopLossSell =
                prepareOrder
                    Nothing
                    (fromJust $ HM.lookup "SHORT_POSITION_STOP_LOSS" stopLossMap)
                    Nothing
                    (Just Buy)
                    (avgCostPrice * 1.05)
                    (Just (avgCostPrice * 1.05 - 1))
                    cumQty
                    Nothing
                    Nothing
        res <- bulkAmendOrders [stopLossSell]
        return ()

initStopLossOrders ::
       Int -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
initStopLossOrders time = do
    let stopLossBuy =
            prepareOrder
                (Just "buystoploss")
                ("buyteststop" <> (T.pack . show) time)
                (Just StopLimit)
                (Just Sell)
                0.5
                (Just 1)
                1
                (Just LastPrice)
                (Just OCO)
        stopLossSell =
            prepareOrder
                (Just "sellstoploss")
                ("sellteststop" <> (T.pack . show) time)
                (Just StopLimit)
                (Just Buy)
                1000000
                (Just 99999)
                1
                (Just LastPrice)
                (Just OCO)
    placeBulkOrder [stopLossBuy, stopLossSell]

placeBulkOrder ::
       [Mex.Order]
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
placeBulkOrder orders = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNewBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orders\": " <> encode orders <> "}"
    makeRequest orderRequest

placeOrder ::
       Mex.Order
    -> BitMEXReader IO (Mex.MimeResult Mex.Order)
placeOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderNew
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) XBTUSD))
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    makeRequest orderRequest

amendOrder ::
       Mex.Order
    -> BitMEXReader IO (Mex.MimeResult Mex.Order)
amendOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderAmend
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    makeRequest orderRequest

bulkAmendOrders ::
       [Mex.Order]
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
bulkAmendOrders orders = do
    print orders
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderAmendBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orders\": " <> encode orders <> "}"
    makeRequest orderRequest

prepareOrder ::
       Maybe Text
    -> Text
    -> Maybe OrderType
    -> Maybe Side
    -> Double
    -> Maybe Double
    -> Double
    -> Maybe ExecutionInstruction
    -> Maybe ContingencyType
    -> Mex.Order
prepareOrder linkId clientId orderType side price stopPx orderQty executionType contingencyType =
    (Mex.mkOrder clientId)
    { Mex.orderSymbol = Just ((T.pack . show) XBTUSD)
    , Mex.orderOrdType = fmap (T.pack . show) orderType
    , Mex.orderClOrdLinkId = linkId
    -- , Mex.orderClOrdId = Just clientId
    , Mex.orderOrderId = clientId
    , Mex.orderSide = fmap (T.pack . show) side
    , Mex.orderPrice = Just price
    , Mex.orderStopPx = stopPx
    , Mex.orderExecInst = fmap (T.pack . show) executionType
    , Mex.orderContingencyType =
          fmap (T.pack . show) contingencyType
    , Mex.orderOrderQty = Just orderQty
    }

makeMarket ::
       Double
    -> Double
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
makeMarket ask bid = do
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    let buyOrder =
            prepareOrder
                (Just "buytest")
                ("buytestlimit" <> (T.pack . show) time)
                (Just Limit)
                (Just Buy)
                bid
                Nothing
                66
                Nothing
                Nothing
        sellOrder =
            prepareOrder
                (Just "selltest")
                ("selltestlimit" <> (T.pack . show) time)
                (Just Limit)
                (Just Sell)
                ask
                Nothing
                66
                Nothing
                Nothing
    placeBulkOrder [buyOrder, sellOrder]
