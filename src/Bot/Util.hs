module Bot.Util
    ( makeMarket
    , prepareOrder
    , placeBulkOrder
    , cancelLimitOrders
    , getLimit
    , getOrderSize
    , waitForOpenOrderChange
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
import qualified BitMEX                         as Mex
    ( Accept (..)
    , BitMEXRequest (..)
    , ContentType (..)
    , Filter (..)
    , Leverage
    , MimeError (..)
    , MimeFormUrlEncoded (..)
    , MimeJSON (..)
    , MimeResult (..)
    , Order (..)
    , Position
    , Symbol (..)
    , applyOptionalParam
    , orderAmend
    , orderAmendBulk
    , orderCancel
    , orderCancelAll
    , orderNew
    , orderNewBulk
    , orderOrdStatusL
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
    , TABLE (..)
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
import           Control.Concurrent
    ( threadDelay
    )
import           Control.Concurrent.STM.TBQueue
    ( TBQueue
    , readTBQueue
    , writeTBQueue
    )
import           Control.Concurrent.STM.TVar
    ( TVar
    , readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader           as R
    ( ask
    , asks
    , runReaderT
    )
import           Control.Monad.STM
    ( STM
    , atomically
    , retry
    )
import           Data.Aeson                     (encode)
import qualified Data.ByteString.Lazy           as LBS
    ( ByteString
    )
import qualified Data.Text                      as T (pack)
import qualified Data.Vector                    as V (head)
import           Lens.Micro
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

-------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------
unWrapBotWith ::
       (BitMEXBot ())
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
       Mex.Order -> BitMEXBot (Mex.MimeResult Mex.Order)
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
       [Text] -> BitMEXBot (Mex.MimeResult [Mex.Order])
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
       [Mex.Order] -> BitMEXBot (Mex.MimeResult [Mex.Order])
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
       Mex.Order -> BitMEXBot (Mex.MimeResult Mex.Order)
amendOrder order = do
    let orderTemplate@(Mex.BitMEXRequest {..}) =
            Mex.orderAmend
                (Mex.ContentType Mex.MimeJSON)
                (Mex.Accept Mex.MimeJSON)
        orderRequest =
            Mex._setBodyLBS orderTemplate $ encode order
    BitMEXBot . lift $ makeRequest orderRequest

bulkAmendOrders ::
       [Mex.Order] -> BitMEXBot (Mex.MimeResult [Mex.Order])
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
       BitMEXBot (Mex.MimeResult Mex.Order) -> BitMEXBot ()
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
        else do
            kill "amending failed"

cancelStopOrder :: OrderID -> BitMEXBot ()
cancelStopOrder (OrderID (Just oid)) = do
    cancelOrders [oid] >> R.asks stopOrderId >>= \o ->
        liftIO $ atomically $ writeTVar o (OrderID Nothing)
cancelStopOrder _ = return ()

cancelLimitOrders :: Text -> BitMEXBot ()
cancelLimitOrders side = do
    let template =
            (Mex.orderCancelAll
                 (Mex.ContentType Mex.MimeJSON)
                 (Mex.Accept Mex.MimeJSON))
        query =
            ( "filter"
            , Just
                  ("{\"ordType\": \"Limit\", \"side\":\"" <>
                   side <>
                   "\"}")) :: (ByteString, Maybe Text)
        req = Mex.setQuery template $ Mex.toQuery query
    Mex.MimeResult {Mex.mimeResult = res} <-
        BitMEXBot . lift $ makeRequest req
    case res of
        Left (Mex.MimeError {mimeError = s}) -> kill s
        Right _                              -> return ()

kill :: String -> BitMEXBot ()
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

restart :: BitMEXBot ()
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
        atomically $ writeTVar openBuyCost 0
        atomically $ writeTVar openSells 0
        atomically $ writeTVar openSellCost 0

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
            "{\"leverage\": " <> encode (Mex.unLeverage lev) <>
            ", \"symbol\": " <>
            (encode $ (T.pack . show) sym) <>
            "}"
    makeRequest leverageRequest

-------------------------------------------------------------
-- MARKET MAKING
-------------------------------------------------------------
getLimit :: Double -> Double -> Integer
getLimit price balance =
    floor $
    (convert XBT_to_USD price) *
    (convert XBt_to_XBT $ balance * 0.35)

getOrderSize :: Double -> Double -> Integer
getOrderSize price balance =
    floor $
    (convert XBT_to_USD price) *
    (convert XBt_to_XBT $ balance * 0.07)

waitForOpenOrderChange :: (Integer, Integer) -> (TVar Integer, TVar Integer) -> STM ()
waitForOpenOrderChange (buyQty, sellQty) (openBuys, openSells) = do
    buyQty' <- readTVar openBuys
    sellQty' <- readTVar openSells
    if (buyQty' /= buyQty || sellQty' /= sellQty)
       then return ()
       else retry

makeMarket ::
       Integer
    -> Integer
    -> Double
    -> Double
    -> BitMEXBot ()
makeMarket limit orderSize ask bid = do
    BotState {..} <- R.ask
    size <- liftIO $ atomically $ readTVar positionSize
    buys' <- liftIO $ atomically $ readTVar openBuys
    sells' <- liftIO $ atomically $ readTVar openSells
    openBC <- liftIO $ atomically $ readTVar openBuyCost
    openSC <- liftIO $ atomically $ readTVar openSellCost
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
                    liftIO $
                        atomically $
                        waitForOpenOrderChange
                            (buys', sells')
                            (openBuys, openSells)
                else if (code == 503 || code == 502)
                         then do
                             liftIO $ threadDelay 500000
                             return ()
                         else kill "order didn't go through"
        else return ()
