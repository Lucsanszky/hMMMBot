module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , placeOrder
    , amendStopOrder
    , cancelStopOrder
    , kill
    , restart
    , placeStopOrder
    , amendOrder
    , cancelOrders
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
    , orderAmend
    , orderAmendBulk
    , orderCancel
    , orderCancelAll
    , orderNew
    , orderNewBulk
    , _setBodyLBS
    )
import           BitMEXClient
    ( BitMEXReader (..)
    , BitMEXWrapperConfig
    , Side (..)
    , Symbol (..)
    , makeRequest
    )
import           Bot.Concurrent
import           Bot.OrderTemplates
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    , OrderID (..)
    , PositionType (..)
    )
import           Control.Concurrent.STM.TVar
    ( readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader        as R
    ( ask
    , asks
    , runReaderT
    )
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (encode)
import qualified Data.Text                   as T (pack)
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

cancelOrders ::
       [Text] -> BitMEXBot IO (Mex.MimeResult [Mex.Order])
cancelOrders ids = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderCancel
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $
            "{\"orderID\": " <> encode ids <> "}"
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

placeStopOrder ::
       BitMEXBot IO (Mex.MimeResult Mex.Order)
    -> BitMEXBot IO ()
placeStopOrder order = do
    Mex.MimeResult {Mex.mimeResult = res} <- order
    case res of
        Left (Mex.MimeError {mimeError = s}) -> do
            kill
            fail s
        Right (Mex.Order {orderOrderId = oid}) ->
            R.asks stopOrderId >>= \o ->
                liftIO $
                atomically $
                writeTVar o (OrderID (Just oid))

amendStopOrder ::
       Maybe Text -> Maybe Double -> BitMEXBot IO ()
amendStopOrder oid stopPx = do
    let newStopLoss =
            (orderWithId (OrderID oid))
            {Mex.orderStopPx = stopPx}
    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
        amendOrder newStopLoss
    let HTTP.Status {statusCode = code} =
            responseStatus resp
    if code == 200
        then return ()
        else do
            kill
            fail "amending failed"

cancelStopOrder :: OrderID -> BitMEXBot IO ()
cancelStopOrder (OrderID (Just oid)) = do
    cancelOrders [oid] >> R.asks stopOrderId >>= \o ->
        liftIO $ atomically $ writeTVar o (OrderID Nothing)
cancelStopOrder _ = return ()

kill :: BitMEXBot IO ()
kill = do
    pSize <-
        R.asks positionSize >>=
        (liftIO . atomically . readTVar)
    restart
    let close =
            if pSize < 0
                then closePosition Buy
                else closePosition Sell
    placeStopOrder (placeOrder close)

restart :: BitMEXBot IO ()
restart = do
    BotState {..} <- R.ask
    BitMEXBot . lift $
        makeRequest
            (Mex.orderCancelAll
                 (Mex.ContentType Mex.MimeJSON)
                 (Mex.Accept Mex.MimeJSON))
    liftIO $ do
        atomically $ writeTVar stopOrderId (OrderID Nothing)
        atomically $ writeTVar positionSize 0
        atomically $ writeTVar prevPosition None
        atomically $ writeTVar openBuys 0
        atomically $ writeTVar openSells 0

-------------------------------------------------------------
-- MARKET MAKING
-------------------------------------------------------------
_MAX_POSITION_ :: Integer
_MAX_POSITION_ = 42

makeMarket ::
       Double
    -> Double
    -> BitMEXBot IO ()
makeMarket ask bid = do
    BotState {..} <- R.ask
    size <- liftIO $ atomically $ readTVar positionSize
    buys' <- liftIO $ atomically $ readTVar openBuys
    sells' <- liftIO $ atomically $ readTVar openSells
    let buys =
            if size > 0
                then size + buys'
                else buys'
        sells =
            if size < 0
                then (abs size) + sells'
                else sells'
    if (buys < _MAX_POSITION_ || sells < _MAX_POSITION_)
        then do
            let (orders, newBuyQty, newSellQty) =
                    if (buys < _MAX_POSITION_ &&
                        sells < _MAX_POSITION_)
                        then ( [limitSell ask, limitBuy bid]
                             , buys + _MAX_POSITION_
                             , sells + _MAX_POSITION_)
                        else if (buys < _MAX_POSITION_)
                                 then ( [limitBuy bid]
                                      , buys +
                                        _MAX_POSITION_
                                      , sells)
                                 else ( [limitSell ask]
                                      , buys
                                      , sells +
                                        _MAX_POSITION_)
            Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                placeBulkOrder orders
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then do
                    liftIO $
                        atomically $
                        updateVar openBuys newBuyQty
                    liftIO $
                        atomically $
                        updateVar openSells newSellQty
                else do
                    kill
                    fail "order didn't go through"
        else return ()
