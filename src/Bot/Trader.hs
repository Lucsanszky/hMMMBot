module Bot.Trader
    ( trader
    ) where

import           BasicPrelude                hiding (id)
import qualified BitMEX                      as Mex
    ( Leverage (..)
    )
import           BitMEXClient
    ( BitMEXWrapperConfig (..)
    , RespOrderBookL2 (..)
    , Side (..)
    )
import           Bot.Math
    ( convert
    , getPrice
    )
import           Bot.Types
    ( BotState (..)
    , Rule (..)
    )
import           Bot.Util
    ( amendLimitOrder
    , getLimit
    , getOrderSize
    , kill
    , makeMarket
    , unWrapBotWith
    )
import           Control.Concurrent.STM.TVar (readTVar)
import           Control.Monad.STM           (atomically)
import           Data.IORef                  (readIORef)
import qualified Data.Vector                 as V
    ( filter
    , head
    , last
    )

trader ::
    Vector RespOrderBookL2
    -> BotState
    -> BitMEXWrapperConfig
    -> IO ()
trader vectorOBL2 botState@BotState {..} config = do
    let buys =
            map (\x -> getPrice $ id (x :: RespOrderBookL2)) $
            V.filter
                (\x -> side (x :: RespOrderBookL2) == Buy)
                vectorOBL2
        sells =
            map (\x -> getPrice $ id (x :: RespOrderBookL2)) $
            V.filter
                (\x -> side (x :: RespOrderBookL2) == Sell)
                vectorOBL2
    bestAsk' <- readIORef bestAsk
    bestBid' <- readIORef bestBid
    sellQty <- readIORef openSells
    buyQty <- readIORef openBuys
    posSize <- readIORef positionSize
    buyID' <- readIORef buyID
    sellID' <- readIORef sellID
    total <- atomically $ readTVar walletBalance
    available <-
        liftIO $ atomically $ readTVar availableBalance
    let orderSize =
            getOrderSize bestAsk' $ fromIntegral total * lev
        lev = Mex.unLeverage leverage
        limit = getLimit bestAsk' $ fromIntegral total * lev
    when (length sells > 0) $ do
        when
            (V.last sells == bestAsk' &&
             (V.head sells - bestBid' < 4.0)) $ do
            when
                (sellQty == 0 && buyQty == 0 && posSize == 0) $ do
                if convert
                       XBt_to_XBT
                       (fromIntegral available) >
                   convert USD_to_XBT bestAsk' *
                   fromIntegral orderSize /
                   lev
                    then unWrapBotWith
                             (makeMarket
                                  "Buy"
                                  limit
                                  orderSize
                                  bestAsk'
                                  (V.head sells))
                             botState
                             config
                    else unWrapBotWith
                             (kill "not enough funds")
                             botState
                             config
            when (sellQty == 0 && buyQty /= 0) $ do
                unWrapBotWith
                    (amendLimitOrder
                         buyID'
                         buyID
                         (Just (V.head sells)))
                    botState
                    config
    when (length buys > 0) $ do
        when
            (V.head buys == bestBid' &&
             (bestAsk' - V.last buys < 4.0)) $ do
            when
                (sellQty == 0 && buyQty == 0 && posSize == 0) $ do
                if convert
                       XBt_to_XBT
                       (fromIntegral available) >
                   convert USD_to_XBT bestAsk' *
                   fromIntegral orderSize /
                   lev
                    then unWrapBotWith
                             (makeMarket
                                  "Sell"
                                  limit
                                  orderSize
                                  (V.last buys)
                                  bestBid')
                             botState
                             config
                    else unWrapBotWith
                             (kill "not enough funds")
                             botState
                             config
            when (sellQty /= 0 && buyQty == 0) $ do
                unWrapBotWith
                    (amendLimitOrder
                         sellID'
                         sellID
                         (Just (V.last buys)))
                    botState
                    config
