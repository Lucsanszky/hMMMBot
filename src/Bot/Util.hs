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
import           BitMEXClient
import           Bot.Concurrent
import           Bot.Math
import           Bot.Types
import           Control.Concurrent.STM.TVar
import qualified Control.Monad.Reader        as R
    ( asks
    , runReaderT
    )
import           Control.Monad.STM
import           Data.Aeson
    ( Value (String)
    , encode
    , toJSON
    )
import qualified Data.HashMap.Strict         as HM
    ( insert
    , lookup
    )
import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T (pack)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Vector                 (head)

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
        Nothing -> order
        Just id -> order {Mex.orderOrderId = id}

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

insertStopLossOrder :: Text -> Text -> BitMEXBot IO ()
insertStopLossOrder id stopLossType = do
    stopMap <- R.asks stopLossMap
    slm <-
        R.asks stopLossMap >>=
        (liftIO . atomically . readTVar)
    liftIO $
        atomically $
        writeTVar stopMap $ HM.insert stopLossType id slm

initStopLossOrders :: Int -> BitMEXBot IO ()
initStopLossOrders time = do
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
                            } ->
                     case oside of
                         Nothing ->
                             fail
                                 "CRITICAL: order side is missing"
                         Just s ->
                             if s == "Buy"
                                 then insertStopLossOrder
                                          oid
                                          "SHORT_POSITION_STOP_LOSS"
                                 else insertStopLossOrder
                                          oid
                                          "LONG_POSITION_STOP_LOSS")
                orders

-------------------------------------------------------------
-- RISK MANAGER
-------------------------------------------------------------
manageRisk :: Double -> Maybe Double -> BitMEXBot IO ()
manageRisk _ Nothing = return ()
manageRisk cumQty (Just avgCostPrice)
    | cumQty > 0 = do
        time <- liftIO $ makeTimestamp <$> getPOSIXTime
        slm <-
            R.asks stopLossMap >>=
            (liftIO . atomically . readTVar)
        let stopLossBuy =
                prepareOrder
                    (HM.lookup "LONG_POSITION_STOP_LOSS" slm)
                    Nothing
                    Nothing
                    Nothing
                    (Just Sell)
                    (Just (roundPrice $ avgCostPrice * 0.95))
                    (Just
                         (roundPrice $
                          avgCostPrice * 0.95 + 1))
                    (Just cumQty)
                    Nothing
                    Nothing
        res <- bulkAmendOrders [stopLossBuy]
        return ()
    | cumQty < 0 = do
        time <- liftIO $ makeTimestamp <$> getPOSIXTime
        slm <-
            R.asks stopLossMap >>=
            (liftIO . atomically . readTVar)
        let stopLossSell =
                prepareOrder
                    (HM.lookup
                         "SHORT_POSITION_STOP_LOSS"
                         slm)
                    Nothing
                    Nothing
                    Nothing
                    (Just Buy)
                    (Just (roundPrice $ avgCostPrice * 1.05))
                    (Just
                         (roundPrice $
                          avgCostPrice * 1.05 - 1))
                    (Just cumQty)
                    Nothing
                    Nothing
        res <- bulkAmendOrders [stopLossSell]
        return ()
    | otherwise = return ()

riskManager :: BotState -> BitMEXWrapperConfig -> IO ()
riskManager botState@BotState {..} config = do
    resp <- atomically $ readResponse positionQueue
    case resp of
        P (TABLE {_data = positionData}) -> do
            let RespPosition { currentQty = qty
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
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
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
