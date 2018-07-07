module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , getAggressiveLimit
    , getPassiveLimit
    , getOrderSize
    , updateLeverage
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
    , Leverage
    , MimeError (..)
    , MimeJSON (..)
    , MimeResult (..)
    , Order (..)
    , Position
    , Symbol (..)
    , orderAmend
    , orderAmendBulk
    , orderCancel
    , orderCancelAll
    , orderNew
    , orderNewBulk
    , orderOrdStatusL
    , orderSideL
    , positionUpdateLeverage
    , unLeverage
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
import           Bot.Math
import           Bot.OrderTemplates
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    , OrderID (..)
    , PositionType (..)
    , Rule (..)
    )
import           Control.Concurrent          (threadDelay)
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
import           Lens.Micro
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
            kill s
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
            kill "amending failed"

cancelStopOrder :: OrderID -> BitMEXBot IO ()
cancelStopOrder (OrderID (Just oid)) = do
    cancelOrders [oid] >> R.asks stopOrderId >>= \o ->
        liftIO $ atomically $ writeTVar o (OrderID Nothing)
cancelStopOrder _ = return ()

kill :: String -> BitMEXBot IO ()
kill msg = do
    pSize <-
        R.asks positionSize >>=
        (liftIO . atomically . readTVar)
    restart
    let close =
            if pSize < 0
                then closePosition Buy
                else closePosition Sell
    placeStopOrder (placeOrder close)
    fail msg

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
-- POSITION
-------------------------------------------------------------
updateLeverage ::
       Symbol
    -> Mex.Leverage
    -> BitMEXReader IO (Mex.MimeResult Mex.Position)
updateLeverage sym lev = do
    let leverageTemplate =
            Mex.positionUpdateLeverage
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) sym))
                lev
        leverageRequest =
            Mex._setBodyLBS leverageTemplate $
            "{\"leverage\": " <> encode (Mex.unLeverage lev) <>
            ", \"symbol\": " <>
            (encode $ (T.pack . show) sym) <>
            "}"
    makeRequest leverageRequest

-------------------------------------------------------------
-- MARKET MAKING
-------------------------------------------------------------
getPassiveLimit :: Double -> Double -> Integer
getPassiveLimit price balance =
    floor $
    (convert XBT_to_USD price) *
    (convert XBt_to_XBT $ balance * 0.075)

getAggressiveLimit :: Double -> Double -> Integer
getAggressiveLimit price balance =
    floor $
    (convert XBT_to_USD price) *
    (convert XBt_to_XBT $ balance * 0.15)

getOrderSize :: Double -> Double -> Integer
getOrderSize price balance =
    floor $
    (convert XBT_to_USD price) *
    (convert XBt_to_XBT $ balance * 0.02)

incrementQty ::
       Integer
    -> Maybe Text
    -> [(Maybe Text, Maybe Text)]
    -> Integer
incrementQty orderSize side =
    sum .
    map (\(stat, side') ->
             if (stat == Just "New" && side == side')
                 then orderSize
                 else 0)

makeMarket ::
       Integer
    -> Integer
    -> Double
    -> Double
    -> BitMEXBot IO ()
makeMarket limit orderSize ask bid = do
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
    if (buys < limit || sells < limit)
        then do
            let orders =
                    if (buys < limit && sells < limit)
                        then [ limitSell
                                   (fromIntegral orderSize)
                                   ask
                             , limitBuy
                                   (fromIntegral orderSize)
                                   bid
                             ]
                        else if (buys < limit)
                                 then [ limitBuy
                                            (fromIntegral
                                                 orderSize)
                                            bid
                                      ]
                                 else [ limitSell
                                            (fromIntegral
                                                 orderSize)
                                            ask
                                      ]
            Mex.MimeResult { Mex.mimeResultResponse = resp
                           , Mex.mimeResult = res
                           } <- placeBulkOrder orders
            let HTTP.Status {statusCode = code} =
                    responseStatus resp
            if code == 200
                then do
                    let Right orders = res
                        pairs =
                            map
                                (\o ->
                                     ( o ^.
                                       Mex.orderOrdStatusL
                                     , o ^. Mex.orderSideL))
                                orders
                    liftIO $
                        atomically $
                        updateVar openBuys $
                        buys' +
                        incrementQty
                            orderSize
                            (Just "Buy")
                            pairs
                    liftIO $
                        atomically $
                        updateVar openSells $
                        sells' +
                        incrementQty
                            orderSize
                            (Just "Sell")
                            pairs
                else if code == 503
                         then do
                             liftIO $ threadDelay 500000
                             return ()
                         else kill "order didn't go through"
        else return ()
