module Bot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
    )
import qualified BitMEX                         as Mex
    ( Accept (..)
    , Leverage (..)
    , Margin (..)
    , MimeJSON (..)
    , MimeResult (..)
    , userGetMargin
    )
import           BitMEXClient
    ( BitMEXApp
    , BitMEXWrapperConfig (..)
    , Command (..)
    , RespExecution (..)
    , RespMargin (..)
    , RespOrderBook10 (..)
    , RespPosition (..)
    , Response (..)
    , Symbol (..)
    , TABLE (..)
    , Topic (..)
    , getMessage
    , makeRequest
    , makeTimestamp
    , sendMessage
    , sign
    )
import           Bot.Math                       (convert)
import           Bot.RiskManager
    ( lossLimitUpdater
    , pnlTracker
    , riskManager
    , stopLossWatcher
    )
import           Bot.Types
    ( BitMEXBot (..)
    , BotState (..)
    , OrderID (..)
    , PnLQueue (..)
    , PositionType (..)
    , RiskManagerQueue (..)
    , Rule (..)
    , StopLossWatcherQueue (..)
    )
import           Bot.Util
    ( amendLimitOrder
    , getLimit
    , getOrderSize
    , kill
    , makeMarket
    , unWrapBotWith
    , updateLeverage
    )
import           Control.Concurrent.Async       (async)
import qualified Control.Concurrent.Async       as A (link)
import           Control.Concurrent.STM.TBQueue
    ( newTBQueue
    , writeTBQueue
    )
import           Control.Concurrent.STM.TVar
    ( newTVar
    , readTVar
    , writeTVar
    )
import qualified Control.Monad.Reader           as R
    ( ask
    , asks
    , lift
    , runReaderT
    )
import           Control.Monad.STM              (atomically)
import           Data.Aeson
    ( Value (String)
    , toJSON
    )
import           Data.ByteString.Char8          (pack)
import           Data.IORef
    ( IORef
    , atomicWriteIORef
    , newIORef
    , readIORef
    )
import           Data.Maybe                     (fromJust)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                    (head, (!?))

resetOrder ::
       BotState
    -> BitMEXWrapperConfig
    -> OrderID
    -> IORef OrderID
    -> Double
    -> IO ()
resetOrder botState config oid idRef price =
    unWrapBotWith
        (amendLimitOrder oid idRef (Just price))
        botState
        config

trader ::
       BotState
    -> BitMEXWrapperConfig
    -> (Double, Double)
    -> (IORef Double, IORef Double)
    -> (IORef OrderID, IORef OrderID)
    -> IO ()
trader botState@BotState {..} config (newBestAsk, newBestBid) (prevAsk, prevBid) (sellID, buyID) = do
    prevAsk' <- readIORef prevAsk
    prevBid' <- readIORef prevBid
    sellQty <- readIORef openSells
    buyQty <- readIORef openBuys
    posSize <- readIORef positionSize
    buyID' <- readIORef buyID
    sellID' <- readIORef sellID
    total <- atomically $ readTVar walletBalance
    let orderSize =
            getOrderSize newBestAsk $
            fromIntegral total * lev
        diff = newBestAsk - newBestBid
        lev = Mex.unLeverage leverage
        limit =
            getLimit newBestAsk $ fromIntegral total * lev
    available <-
        liftIO $ atomically $ readTVar availableBalance
    if convert XBt_to_XBT (fromIntegral available) >
       convert USD_to_XBT newBestAsk *
       fromIntegral orderSize /
       lev
        then do
            if (newBestBid < prevBid' &&
                buyQty == 0 && sellQty == 0 && posSize == 0)
                then do
                    let ask
                            | diff > 0.5 = newBestBid + 0.5
                            | otherwise = newBestAsk
                    unWrapBotWith
                        (makeMarket
                             "Sell"
                             limit
                             orderSize
                             ask
                             newBestBid
                             (sellID, buyID))
                        botState
                        config
                    atomicWriteIORef prevAsk ask
                    atomicWriteIORef prevBid newBestBid
                else if (newBestAsk > prevAsk' &&
                         sellQty == 0 &&
                         buyQty == 0 && posSize == 0)
                         then do
                             let bid
                                     | diff > 0.5 =
                                         newBestAsk - 0.5
                                     | otherwise =
                                         newBestBid
                             unWrapBotWith
                                 (makeMarket
                                      "Buy"
                                      limit
                                      orderSize
                                      newBestAsk
                                      bid
                                      (sellID, buyID))
                                 botState
                                 config
                             atomicWriteIORef
                                 prevAsk
                                 newBestAsk
                             atomicWriteIORef prevBid bid
                         else if (posSize > 0 &&
                                  sellQty == 0)
                                  then do
                                      let ask
                                              | diff > 0.5 =
                                                  newBestBid +
                                                  0.5
                                              | otherwise =
                                                  newBestAsk
                                      unWrapBotWith
                                          (makeMarket
                                               "Sell"
                                               limit
                                               orderSize
                                               ask
                                               newBestBid
                                               ( sellID
                                               , buyID))
                                          botState
                                          config
                                      atomicWriteIORef
                                          prevAsk
                                          ask
                                      atomicWriteIORef
                                          prevBid
                                          newBestBid
                                  else if (posSize < 0 &&
                                           buyQty == 0)
                                           then do
                                               let bid
                                                       | diff >
                                                             0.5 =
                                                           newBestAsk -
                                                           0.5
                                                       | otherwise =
                                                           newBestBid
                                               unWrapBotWith
                                                   (makeMarket
                                                        "Buy"
                                                        limit
                                                        orderSize
                                                        newBestAsk
                                                        bid
                                                        ( sellID
                                                        , buyID))
                                                   botState
                                                   config
                                               atomicWriteIORef
                                                   prevAsk
                                                   newBestAsk
                                               atomicWriteIORef
                                                   prevBid
                                                   bid
                                           else do
                                               atomicWriteIORef
                                                   prevAsk
                                                   newBestAsk
                                               atomicWriteIORef
                                                   prevBid
                                                   newBestBid
        else unWrapBotWith
                 (kill "not enough funds")
                 botState
                 config
    when
        (sellQty == 0 &&
         buyQty /= 0 && newBestBid >= prevBid') $ do
        if diff > 0.5
                -- Don't amend if the bot has already done so.
                -- I.e.: previous value was updated locally,
                -- thus it differs from the exchange's value
            then when (prevBid' == newBestBid) $ do
                     resetOrder
                         botState
                         config
                         buyID'
                         buyID
                         (newBestAsk - 0.5)
                     atomicWriteIORef
                         prevBid
                         (newBestAsk - 0.5)
                     atomicWriteIORef prevAsk newBestAsk
            else do
                resetOrder
                    botState
                    config
                    buyID'
                    buyID
                    newBestBid
                atomicWriteIORef prevAsk newBestAsk
                atomicWriteIORef prevBid newBestBid
        return ()
    when
        (buyQty == 0 &&
         sellQty /= 0 && newBestAsk <= prevAsk') $ do
        if diff > 0.5
                -- Don't amend if the bot has already done so.
                -- I.e.: previous value was updated locally,
                -- thus it differs from the exchange's value
            then when (prevAsk' == newBestAsk) $ do
                     resetOrder
                         botState
                         config
                         sellID'
                         sellID
                         (newBestBid + 0.5)
                     atomicWriteIORef
                         prevAsk
                         (newBestBid + 0.5)
                     atomicWriteIORef prevBid newBestBid
            else do
                resetOrder
                    botState
                    config
                    sellID'
                    sellID
                    newBestAsk
                atomicWriteIORef prevBid newBestBid
                atomicWriteIORef prevAsk newBestAsk
        return ()

tradeLoop :: BitMEXBot ()
tradeLoop = do
    config <- BitMEXBot $ R.lift R.ask
    botState@BotState {..} <- R.ask
    liftIO $ do
        risk <-
            async $ forever $ riskManager botState config
        slw <-
            async $
            forever $ stopLossWatcher botState config
        pnl <- async $ forever $ pnlTracker botState config
        loss <-
            async $
            forever $ lossLimitUpdater botState config
        mapM_ A.link [risk, slw, pnl, loss]
    loop
  where
    loop = loop

processResponse ::
       BotState
    -> BitMEXWrapperConfig
    -> (IORef Double, IORef Double)
    -> (IORef OrderID, IORef OrderID)
    -> Maybe Response
    -> IO ()
processResponse botState@BotState {..} config prevPrices ids msg =
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 TABLE {_data = orderBookData} -> do
                    let RespOrderBook10 { asks = newAsks
                                        , bids = newBids
                                        } =
                            head orderBookData
                        newBestAsk = head $ head newAsks
                        newBestBid = head $ head newBids
                    trader
                        botState
                        config
                        (newBestAsk, newBestBid)
                        prevPrices
                        ids
                posResp@(P TABLE {_data = positionData}) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderBuyCost = buyCost
                                     , openOrderSellQty = sellQty
                                     , openOrderSellCost = sellCost
                                     } = head positionData
                    when (isJust currQty) $ do
                        let q = fromJust $ map floor currQty
                        atomicWriteIORef positionSize q
                        atomically $
                            writeTBQueue
                                (unRiskManagerQueue
                                     riskManagerQueue)
                                (Just posResp)
                    forM_ buyQty $ atomicWriteIORef openBuys
                    forM_ buyCost $
                        atomicWriteIORef openBuyCost
                    forM_ sellQty $
                        atomicWriteIORef openSells
                    forM_ sellCost $
                        atomicWriteIORef openSellCost
                M TABLE {_data = marginData} -> do
                    let RespMargin { realisedPnl = rpnl
                                   , availableMargin = ab
                                   , walletBalance = wb
                                   } = head marginData
                    when (isJust rpnl) $
                        atomically $
                        writeTVar realPnl $ fromJust rpnl
                    when (isJust ab) $
                        atomically $
                        writeTVar availableBalance $
                        fromJust ab
                    when (isJust wb) $
                        atomically $
                        writeTVar walletBalance $
                        fromJust wb
                execResp@(Exe TABLE {_data = execData}) ->
                    case execData !? 0 of
                        Nothing -> return ()
                        Just RespExecution {triggered = text} ->
                            when
                                (text ==
                                 Just "StopOrderTriggered") $
                            atomically $
                            writeTBQueue
                                (unSLWQueue slwQueue)
                                (Just execResp)
                _ -> return ()

initBot :: Mex.Leverage -> BitMEXApp ()
initBot leverage conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    Mex.MimeResult {Mex.mimeResult = res} <-
        makeRequest $
        Mex.userGetMargin (Mex.Accept Mex.MimeJSON)
    let Right Mex.Margin { Mex.marginWalletBalance = Just wb
                         , Mex.marginAvailableMargin = Just ab
                         } = res
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 1
    slwQueue <- liftIO $ atomically $ newTBQueue 1
    pnlQueue <- liftIO $ atomically $ newTBQueue 1
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ newIORef 0
    realPnl <- liftIO $ atomically $ newTVar 0
    prevBalance <- liftIO $ atomically $ newTVar $ floor wb
    availableBalance <-
        liftIO $ atomically $ newTVar $ floor ab
    walletBalance <-
        liftIO $ atomically $ newTVar $ floor wb
    openBuys <- liftIO $ newIORef 0
    openBuyCost <- liftIO $ newIORef 0
    openSells <- liftIO $ newIORef 0
    openSellCost <- liftIO $ newIORef 0
    prevBid <- liftIO $ newIORef 0.0
    prevAsk <- liftIO $ newIORef 99999999999.0
    sellID <- liftIO $ newIORef (OrderID Nothing)
    buyID <- liftIO $ newIORef (OrderID Nothing)
    stopOrderId <-
        liftIO $ atomically $ newTVar (OrderID Nothing)
    let botState =
            BotState
            { connection = conn
            , riskManagerQueue =
                  RiskManagerQueue riskManagerQueue
            , slwQueue = StopLossWatcherQueue slwQueue
            , pnlQueue = PnLQueue pnlQueue
            , prevPosition = prevPosition
            , positionSize = positionSize
            , realPnl = realPnl
            , prevBalance = prevBalance
            , availableBalance = availableBalance
            , walletBalance = walletBalance
            , openBuys = openBuys
            , openBuyCost = openBuyCost
            , openSells = openSells
            , openSellCost = openSellCost
            , stopOrderId = stopOrderId
            , leverage = leverage
            }
    _ <- updateLeverage XBTUSD leverage
    liftIO $ do
        sendMessage
            conn
            AuthKey
            [String pub, toJSON time, (toJSON . show) sig]
        sendMessage
            conn
            Subscribe
            ([ OrderBook10 XBTUSD
             , Execution
             , Position
             , Margin
             ] :: [Topic Symbol])
        processor <-
            async $
            forever $ do
                msg <- getMessage conn config
                processResponse
                    botState
                    config
                    (prevAsk, prevBid)
                    (sellID, buyID)
                    msg
        A.link processor
    R.runReaderT (runBot tradeLoop) botState
