module Bot.Trader
    ( trader
    ) where

import           BasicPrelude                hiding (head)
import qualified BitMEX                      as Mex
    ( Leverage (..)
    )

import           BitMEXClient
    ( BitMEXWrapperConfig (..)
    )
import           Bot.Math                    (convert)
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
import           Data.IORef
    ( atomicWriteIORef
    , readIORef
    )

trader ::
       BotState
    -> BitMEXWrapperConfig
    -> (Double, Double)
    -> IO ()
trader botState@BotState {..} config (newBestAsk, newBestBid) = do
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
        atomicWriteIORef prevAsk (newBestBid + 0.5)
        atomicWriteIORef prevBid newBestBid
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
        atomicWriteIORef prevAsk newBestAsk
        atomicWriteIORef prevBid (newBestAsk - 0.5)
    when (prevAsk' /= newBestAsk || prevBid' /= newBestBid) $ do
        available <-
            liftIO $ atomically $ readTVar availableBalance
        if convert XBt_to_XBT (fromIntegral available) >
           convert USD_to_XBT newBestAsk *
           fromIntegral orderSize /
           lev
            then do
                when
                    (newBestBid /= prevBid' &&
                     buyQty == 0 &&
                     sellQty == 0 && posSize == 0) $ do
                    if (newBestBid < prevBid')
                        then do
                            unWrapBotWith
                                (makeMarket
                                     "Sell"
                                     limit
                                     orderSize
                                     (newBestBid + 0.5)
                                     newBestBid)
                                botState
                                config
                            atomicWriteIORef
                                prevAsk
                                (newBestBid + 0.5)
                            atomicWriteIORef
                                prevBid
                                newBestBid
                        else do
                            atomicWriteIORef
                                prevAsk
                                newBestAsk
                            atomicWriteIORef
                                prevBid
                                newBestBid
                when
                    (newBestAsk /= prevAsk' &&
                     sellQty == 0 &&
                     buyQty == 0 && posSize == 0) $ do
                    if (newBestAsk > prevAsk')
                        then do
                            unWrapBotWith
                                (makeMarket
                                     "Buy"
                                     limit
                                     orderSize
                                     newBestAsk
                                     (newBestAsk - 0.5))
                                botState
                                config
                            atomicWriteIORef
                                prevAsk
                                newBestAsk
                            atomicWriteIORef
                                prevBid
                                (newBestAsk - 0.5)
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
            unWrapBotWith
                (amendLimitOrder
                     buyID'
                     buyID
                     (Just (newBestAsk - 0.5)))
                botState
                config
            atomicWriteIORef prevAsk newBestAsk
            atomicWriteIORef prevBid (newBestAsk - 0.5)
        when
            (buyQty == 0 &&
             sellQty /= 0 && newBestAsk <= prevAsk') $ do
            unWrapBotWith
                (amendLimitOrder
                     sellID'
                     sellID
                     (Just (newBestBid + 0.5)))
                botState
                config
            atomicWriteIORef prevAsk (newBestBid + 0.5)
            atomicWriteIORef prevBid newBestBid
