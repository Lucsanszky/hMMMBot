module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , amendLimitOrder
    , cancelLimitOrders
    , getLimit
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

import           BasicPrelude                hiding (id)
import qualified BitMEX                      as Mex
    ( Accept (..)
    , BitMEXRequest (..)
    , ContentType (..)
    , Error (..)
    , Leverage
    , MimeJSON (..)
    , MimeResult (..)
    , Order (..)
    , Position
    , Symbol (..)
    , errorErrorL
    , errorErrorMessageL
    , orderAmend
    , orderAmendBulk
    , orderCancel
    , orderCancelAll
    , orderNew
    , orderNewBulk
    , orderOrdStatusL
    , orderOrderIdL
    , orderSideL
    , positionUpdateLeverage
    , setQuery
    , toQuery
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
import           Bot.Math                    (convert)
import           Bot.OrderTemplates
    ( closePosition
    , limitBuy
    , limitSell
    , orderWithId
    , prepareOrder
    )
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    , OrderID (..)
    , PositionType (..)
    , Rule (..)
    )
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM.TVar (writeTVar)
import qualified Control.Monad.Reader        as R
    ( ask
    , asks
    , runReaderT
    )
import           Control.Monad.STM           (atomically)
import           Data.Aeson
    ( decode
    , encode
    )
import           Data.IORef
    ( IORef
    , atomicWriteIORef
    , readIORef
    )
import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T (pack)
import           Lens.Micro                  ((^.))
import           Network.HTTP.Client
    ( responseBody
    , responseStatus
    )
import qualified Network.HTTP.Types.Status   as HTTP
    ( Status (..)
    )

-------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------
unWrapBotWith ::
       BitMEXBot ()
    -> BotState
    -> BitMEXWrapperConfig
    -> IO ()
unWrapBotWith f botState =
    R.runReaderT (run (R.runReaderT (runBot f) botState))

-------------------------------------------------------------
-- ORDERS
-------------------------------------------------------------
placeOrder ::
       Mex.Order -> BitMEXBot (Mex.MimeResult Mex.Order)
placeOrder order = do
    let orderTemplate@Mex.BitMEXRequest {..} =
            Mex.orderNew
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) XBTUSD))
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    BitMEXBot . lift $ makeRequest orderRequest

cancelOrders ::
       [Text] -> BitMEXBot (Mex.MimeResult [Mex.Order])
cancelOrders ids = do
    let orderTemplate@Mex.BitMEXRequest {..} =
            Mex.orderCancel
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ "{\"orderID\": " <>
            encode ids <>
            "}"
    BitMEXBot . lift $ makeRequest orderRequest

updateIDs ::
       (IORef OrderID, IORef OrderID)
    -> [(Text, Maybe Text)]
    -> IO ()
updateIDs (sellID, buyID) =
    mapM_
        (\(id, side) ->
             case side of
                 Just "Buy" ->
                     atomicWriteIORef
                         buyID
                         (OrderID (Just id))
                 Just "Sell" ->
                     atomicWriteIORef
                         sellID
                         (OrderID (Just id))
                 Just _ -> return ()
                 Nothing -> return ())

placeBulkOrder ::
       [Mex.Order]
    -> Integer
    -> Double
    -> Double
    -> (IORef OrderID, IORef OrderID)
    -> BitMEXBot ()
placeBulkOrder orders orderSize ask bid ids = do
    obs <- R.asks openBuys
    obc <- R.asks openBuyCost
    oss <- R.asks openSells
    osc <- R.asks openSellCost
    buys' <- liftIO $ readIORef obs
    sells' <- liftIO $ readIORef oss
    openBC <- liftIO $ readIORef obc
    openSC <- liftIO $ readIORef osc
    let orderTemplate@Mex.BitMEXRequest {..} =
            Mex.orderNewBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ "{\"orders\": " <>
            encode orders <>
            "}"
    Mex.MimeResult { Mex.mimeResultResponse = resp
                   , Mex.mimeResult = res
                   } <-
        BitMEXBot . lift $ makeRequest orderRequest
    let HTTP.Status {statusCode = code} =
            responseStatus resp
    if code == 200
        then do
            let Right resOrders = res
                pairs =
                    map
                        (\o ->
                             ( o ^. Mex.orderOrdStatusL
                             , o ^. Mex.orderSideL))
                        resOrders
                ids' =
                    map
                        (\o ->
                             ( o ^. Mex.orderOrderIdL
                             , o ^. Mex.orderSideL))
                        resOrders
            liftIO $ updateIDs ids ids'
            liftIO $ atomicWriteIORef obs $ buys' +
                incrementQty orderSize (Just "Buy") pairs
            liftIO $ atomicWriteIORef obc $ openBC -
                incrementQty
                    (floor $
                     convert
                         XBT_to_XBt
                         (fromIntegral orderSize / bid))
                    (Just "Buy")
                    pairs
            liftIO $ atomicWriteIORef oss $ sells' +
                incrementQty orderSize (Just "Sell") pairs
            liftIO $ atomicWriteIORef osc $ openSC -
                incrementQty
                    (floor $
                     convert
                         XBT_to_XBt
                         (fromIntegral orderSize / ask))
                    (Just "Sell")
                    pairs
        else if code == 503 || code == 502
                 then do
                     liftIO $ threadDelay 250000
                     placeBulkOrder
                         orders
                         orderSize
                         ask
                         bid
                         ids
                 else kill "order didn't go through"

amendOrder ::
       Mex.Order -> BitMEXBot (Mex.MimeResult Mex.Order)
amendOrder order = do
    let orderTemplate@Mex.BitMEXRequest {..} =
            Mex.orderAmend
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    BitMEXBot . lift $ makeRequest orderRequest

amendLimitOrder ::
       OrderID
    -> IORef OrderID
    -> Maybe Double
    -> BitMEXBot ()
amendLimitOrder cid@(OrderID (Just _)) idRef price = do
    let newStopLoss =
            (orderWithId cid) {Mex.orderPrice = price}
    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
        amendOrder newStopLoss
    let HTTP.Status {statusCode = code} =
            responseStatus resp
    if code == 200
        then return ()
        else if code == 400
                 then do
                     let err =
                             decode $ responseBody resp :: Maybe Mex.Error
                         errMsg =
                             fromJust err ^. Mex.errorErrorL .
                             Mex.errorErrorMessageL
                     if errMsg == Just "Invalid ordStatus"
                         then do
                             liftIO $
                                 atomicWriteIORef
                                     idRef
                                     (OrderID Nothing)
                             return ()
                         else kill
                                  "amending limit order failed"
                 else if code == 503 || code == 502
                          then do
                              liftIO $ threadDelay 250000
                              return ()
                          else kill
                                   "amending limit order failed"
amendLimitOrder (OrderID Nothing) _ _ = return ()

bulkAmendOrders ::
       [Mex.Order] -> BitMEXBot (Mex.MimeResult [Mex.Order])
bulkAmendOrders orders = do
    let orderTemplate@Mex.BitMEXRequest {..} =
            Mex.orderAmendBulk
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ "{\"orders\": " <>
            encode orders <>
            "}"
    BitMEXBot . lift $ makeRequest orderRequest

placeStopOrder ::
       BitMEXBot (Mex.MimeResult Mex.Order) -> BitMEXBot ()
placeStopOrder order = do
    Mex.MimeResult {Mex.mimeResult = res} <- order
    case res of
        Left _ -> placeStopOrder order
        Right Mex.Order {orderOrderId = oid} ->
            R.asks stopOrderId >>= \o ->
                liftIO $ atomically $
                writeTVar o (OrderID (Just oid))

amendStopOrder :: Maybe Text -> Maybe Double -> BitMEXBot ()
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
        else if code == 400
                 then do
                     let err =
                             decode $ responseBody resp :: Maybe Mex.Error
                         errMsg =
                             fromJust err ^. Mex.errorErrorL .
                             Mex.errorErrorMessageL
                     if errMsg == Just "Invalid ordStatus"
                         then return ()
                         else amendStopOrder oid stopPx
                 else if code == 503 || code == 502
                          then do
                              liftIO $ threadDelay 500000
                              amendStopOrder oid stopPx
                          else kill
                                   "amending stop order failed"

cancelStopOrder :: OrderID -> BitMEXBot ()
cancelStopOrder so@(OrderID (Just oid)) = do
    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
        cancelOrders [oid]
    let HTTP.Status {statusCode = code} =
            responseStatus resp
    if code == 200
        then R.asks stopOrderId >>= \o ->
                 liftIO $ atomically $
                 writeTVar o (OrderID Nothing)
        else if code == 400
                 then do
                     let err =
                             decode $ responseBody resp :: Maybe Mex.Error
                         errMsg =
                             fromJust err ^. Mex.errorErrorL .
                             Mex.errorErrorMessageL
                     if errMsg == Just "Invalid ordStatus"
                         then R.asks stopOrderId >>= \o ->
                                  liftIO $ atomically $
                                  writeTVar
                                      o
                                      (OrderID Nothing)
                         else cancelStopOrder so
                 else kill "cancelling stop order failed"
cancelStopOrder _ = return ()

cancelLimitOrders :: Text -> BitMEXBot ()
cancelLimitOrders side = do
    let template =
            Mex.orderCancelAll
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        query =
            ( "filter"
            , Just
                  ("{\"ordType\": \"Limit\", \"side\":\"" <>
                   side <>
                   "\"}")) :: (ByteString, Maybe Text)
        req = Mex.setQuery template $ Mex.toQuery query
    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
        BitMEXBot . lift $ makeRequest req
    let HTTP.Status {statusCode = code} =
            responseStatus resp
    if code == 200
        then if side == "Buy"
                 then do
                     openBuys <- R.asks openBuys
                     openBuyCost <- R.asks openBuyCost
                     liftIO $ atomicWriteIORef openBuys 0
                     liftIO $ atomicWriteIORef openBuyCost 0
                 else do
                     openSells <- R.asks openSells
                     openSellCost <- R.asks openSellCost
                     liftIO $ atomicWriteIORef openSells 0
                     liftIO $
                         atomicWriteIORef openSellCost 0
        else if code == 400
                 then do
                     let err =
                             decode $ responseBody resp :: Maybe Mex.Error
                         errMsg =
                             fromJust err ^. Mex.errorErrorL .
                             Mex.errorErrorMessageL
                     if errMsg == Just "Invalid ordStatus"
                         then return ()
                         else cancelLimitOrders side
                 else kill "cancelling limit orders failed"

kill :: String -> BitMEXBot ()
kill msg = do
    pSize <- R.asks positionSize >>= (liftIO . readIORef)
    restart
    let close =
            if pSize < 0
                then closePosition Buy
                else closePosition Sell
    placeStopOrder (placeOrder close)
    fail msg

restart :: BitMEXBot ()
restart = do
    BotState {..} <- R.ask
    _ <-
        BitMEXBot . lift $
        makeRequest
            (Mex.orderCancelAll
                 (Mex.ContentType Mex.MimeJSON)
                 (Mex.Accept Mex.MimeJSON))
    liftIO $ do
        atomically $ writeTVar stopOrderId (OrderID Nothing)
        atomicWriteIORef positionSize 0
        atomically $ writeTVar prevPosition None
        atomicWriteIORef openBuys 0
        atomicWriteIORef openBuyCost 0
        atomicWriteIORef openSells 0
        atomicWriteIORef openSellCost 0

-------------------------------------------------------------
-- POSITION
-------------------------------------------------------------
updateLeverage ::
       Symbol
    -> Mex.Leverage
    -> BitMEXReader (Mex.MimeResult Mex.Position)
updateLeverage sym lev = do
    let leverageTemplate =
            Mex.positionUpdateLeverage
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
                (Mex.Symbol ((T.pack . show) sym))
                lev
        leverageRequest =
            Mex._setBodyLBS leverageTemplate $
            "{\"leverage\": " <>
            encode (Mex.unLeverage lev) <>
            ", \"symbol\": " <>
            encode ((T.pack . show) sym) <>
            "}"
    makeRequest leverageRequest

-------------------------------------------------------------
-- MARKET MAKING
-------------------------------------------------------------
getLimit :: Double -> Double -> Integer
getLimit price balance =
    floor $ convert XBT_to_USD price *
    convert XBt_to_XBT (balance * 0.2)

getOrderSize :: Double -> Double -> Integer
getOrderSize price balance =
    floor $ convert XBT_to_USD price *
    convert XBt_to_XBT (balance * 0.21)

incrementQty ::
       Integer
    -> Maybe Text
    -> [(Maybe Text, Maybe Text)]
    -> Integer
incrementQty orderSize side =
    sum .
    map (\(stat, side') ->
             if stat == Just "New" && side == side'
                 then orderSize
                 else 0)

makeMarket ::
       Integer
    -> Integer
    -> Double
    -> Double
    -> (IORef OrderID, IORef OrderID)
    -> BitMEXBot ()
makeMarket limit orderSize ask bid (sellID, buyID) = do
    BotState {..} <- R.ask
    size <- liftIO $ readIORef positionSize
    buys' <- liftIO $ readIORef openBuys
    sells' <- liftIO $ readIORef openSells
    let buys =
            if size > 0
                then size + buys'
                else buys'
        sells =
            if size < 0
                then abs size + sells'
                else sells'
    when (buys < limit || sells < limit) $ do
        let orders
                | buys < limit && sells < limit =
                    [ limitSell
                          Nothing
                          (fromIntegral orderSize)
                          ask
                    , limitBuy
                          Nothing
                          (fromIntegral orderSize)
                          bid
                    ]
                | buys < limit =
                    [ limitBuy
                          Nothing
                          (fromIntegral orderSize)
                          bid
                    ]
                | otherwise =
                    [ limitSell
                          Nothing
                          (fromIntegral orderSize)
                          ask
                    ]
        placeBulkOrder
            orders
            orderSize
            ask
            bid
            (sellID, buyID)
