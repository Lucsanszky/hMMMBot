module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    ) where

import           BasicPrelude
import qualified BitMEX                as Mex
import           BitMEXClient
import           Data.Aeson
    ( Value (String)
    , encode
    , toJSON
    )
import qualified Data.Text             as T (pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)



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

prepareOrder ::
       Text
    -> Text
    -> OrderType
    -> Side
    -> Double
    -> Maybe Double
    -> Maybe ExecutionInstruction
    -> Maybe ContingencyType
    -> Mex.Order
prepareOrder linkId clientId orderType side price stopPx executionType contingencyType =
    (Mex.mkOrder clientId)
    { Mex.orderSymbol = Just ((T.pack . show) XBTUSD)
    , Mex.orderOrdType = Just ((T.pack . show) orderType)
    , Mex.orderClOrdLinkId = Just linkId
    , Mex.orderClOrdId = Just clientId
    , Mex.orderSide = Just ((T.pack . show) side)
    , Mex.orderPrice = Just price
    , Mex.orderStopPx = stopPx
    , Mex.orderExecInst = fmap (T.pack . show) executionType
    , Mex.orderContingencyType =
          fmap (T.pack . show) contingencyType
    , Mex.orderOrderQty = Just 66
    }
    -- trade botState (askPrice, bidPrice)

makeMarket ::
       Double
    -> Double
    -> BitMEXReader IO (Mex.MimeResult [Mex.Order])
makeMarket ask bid = do
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    let buyOrder =
            prepareOrder
                "buytest"
                ("buytestlimit" <> (T.pack . show) time)
                Limit
                Buy
                bid
                Nothing
                Nothing
                Nothing
        sellOrder =
            prepareOrder
                "selltest"
                ("selltestlimit" <> (T.pack . show) time)
                Limit
                Sell
                ask
                Nothing
                Nothing
                Nothing
    -- Don't post stop orders. Instead during setup, create 2 placeholder limit loss
    -- orders (buy and sell) and amend them based on position change.
        -- stopLossBuy =
        --     prepareOrder
        --         "buytest"
        --         ("buyteststop" <> (T.pack . show) time)
        --         StopLimit
        --         Sell
        --         (bid - 10.0)
        --         (Just (bid - 9.0))
        --         (Just LastPrice)
        --         (Just OCO)
        -- stopLossSell =
        --     prepareOrder
        --         "selltest"
        --         ("sellteststop" <> (T.pack . show) time)
        --         StopLimit
        --         Buy
        --         (ask + 10.0)
        --         (Just (ask + 9.0))
        --         (Just LastPrice)
        --         (Just OCO)
    placeBulkOrder [buyOrder, sellOrder]
