module Bot.Trader
    ( trader
    ) where

import           BasicPrelude                hiding (id)
import qualified BitMEX                      as Mex
    ( Leverage (..)
    )
import           BitMEXClient
    ( BitMEXWrapperConfig (..)
    , RespOrderBook10 (..)
    , Side (..)
    )
import           Bot.Math
    ( convert
    , getPrice
    )
import           Bot.Types
    ( BotState (..)
    , OrderID (..)
    , Rule (..)
    )
import           Bot.Util
    ( amendLimitOrder
    , cancelLimitOrders
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
       Vector RespOrderBook10
    -> BotState
    -> BitMEXWrapperConfig
    -> IO ()
trader vectorOB10 botState@BotState {..} config = do
    sellQty <- readIORef openSells
    buyQty <- readIORef openBuys
    posSize <- readIORef positionSize
    buyID' <- readIORef buyID
    sellID' <- readIORef sellID
    total <- atomically $ readTVar walletBalance
    let RespOrderBook10 {asks = newAsks, bids = newBids} =
            V.head vectorOB10
        newBestAsk = V.head $ V.head newAsks
        newBestBid = V.head $ V.head newBids
        bestAskVol = V.last $ V.head newAsks
        bestBidVol = V.last $ V.head newBids
        orderSize =
            getOrderSize newBestAsk $
            fromIntegral total * lev
        lev = Mex.unLeverage leverage
        limit =
            getLimit newBestAsk $ fromIntegral total * lev
    when (posSize > 0 && sellQty == 0) $ do
        unWrapBotWith
            (makeMarket
                 "Sell"
                 limit
                 orderSize
                 (newBestBid + 0.5)
                 newBestBid)
            botState
            config
    when (posSize < 0 && buyQty == 0) $ do
        unWrapBotWith
            (makeMarket
                 "Buy"
                 limit
                 orderSize
                 newBestAsk
                 (newBestAsk - 0.5))
            botState
            config
    when (bestAskVol <= 5000 || bestBidVol <= 5000) $ do
        available <-
            liftIO $ atomically $ readTVar availableBalance
        if convert XBt_to_XBT (fromIntegral available) >
           convert USD_to_XBT newBestAsk *
           fromIntegral orderSize /
           lev
            then do
                when
                    (bestBidVol <= 5000 &&
                     buyQty == 0 &&
                     sellQty == 0 && posSize == 0) $
                    unWrapBotWith
                        (makeMarket
                             "Sell"
                             limit
                             orderSize
                             newBestBid
                             newBestBid)
                        botState
                        config
                when
                    (bestAskVol <= 5000 &&
                     sellQty == 0 &&
                     buyQty == 0 && posSize == 0) $
                    unWrapBotWith
                        (makeMarket
                             "Buy"
                             limit
                             orderSize
                             newBestAsk
                             newBestAsk)
                        botState
                        config
            else unWrapBotWith
                     (kill "not enough funds")
                     botState
                     config
        -- when
        --     (sellQty == 0 &&
        --      buyQty /= 0 && bestAskVol <= 5000) $ do
        --     unWrapBotWith
        --         (amendLimitOrder
        --              buyID'
        --              buyID
        --              (Just newBestAsk)
        --              5)
        --         botState
        --         config
        -- when
        --     (buyQty == 0 &&
        --      sellQty /= 0 && bestBidVol <= 5000) $ do
        --     unWrapBotWith
        --         (amendLimitOrder
        --              sellID'
        --              sellID
        --              (Just newBestBid)
        --              5)
        --         botState
        --         config
