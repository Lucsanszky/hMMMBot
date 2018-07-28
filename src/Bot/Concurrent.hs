module Bot.Concurrent
    ( readResponse
    , updateVar
    , processResponse
    ) where

import           BasicPrelude
import           BitMEXClient
    ( Action (..)
    , BitMEXWrapperConfig (..)
    , RespExecution (..)
    , RespMargin (..)
    , RespOrderBook10 (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    )
import           Bot.Trader                     (trader)
import           Bot.Types
    ( BotState (..)
    , RiskManagerQueue (..)
    , StopLossWatcherQueue (..)
    )
import           Control.Concurrent.STM.TBQueue
    ( writeTBQueue
    )
import           Control.Concurrent.STM.TBQueue
    ( TBQueue
    , readTBQueue
    )
import           Control.Concurrent.STM.TVar
    ( TVar
    , readTVar
    , writeTVar
    )
import           Control.Monad.STM              (atomically)
import           Control.Monad.STM              (STM, retry)
import           Data.Maybe                     (fromJust)
import qualified Data.Vector                    as V
    ( head
    , (!?)
    )

processResponse ::
       Maybe Response
    -> BotState
    -> BitMEXWrapperConfig
    -> IO ()
processResponse Nothing _ _ = return ()
processResponse (Just (OB TABLE { _action = Delete
                                , _data = orderBookData
                                })) botState config =
    trader orderBookData botState config
processResponse (Just (OB10 TABLE {_data = orderBookData})) BotState {..} _ = do
    let RespOrderBook10 {asks = newAsks, bids = newBids} =
            V.head orderBookData
    atomically $ writeTVar bestAsk $ V.head $ V.head newAsks
    atomically $ writeTVar bestBid $ V.head $ V.head newBids
processResponse (Just posResp@(P TABLE {_data = positionData})) BotState {..} _ = do
    let RespPosition { currentQty = currQty
                     , openOrderBuyQty = buyQty
                     , openOrderBuyCost = buyCost
                     , openOrderSellQty = sellQty
                     , openOrderSellCost = sellCost
                     } = V.head positionData
    when (isJust currQty) $ do
        let q = fromJust $ map floor currQty
        atomically $ writeTVar positionSize q
        atomically $
            writeTBQueue
                (unRiskManagerQueue riskManagerQueue)
                (Just posResp)
    atomically $ forM_ buyQty $ writeTVar openBuys
    atomically $ forM_ buyCost $ writeTVar openBuyCost
    atomically $ forM_ sellQty $ writeTVar openSells
    atomically $ forM_ sellCost $ writeTVar openSellCost
processResponse (Just (M TABLE {_data = marginData})) BotState {..} _ = do
    let RespMargin { realisedPnl = rpnl
                   , availableMargin = ab
                   , walletBalance = wb
                   } = V.head marginData
    when (isJust rpnl) $
        atomically $ writeTVar realPnl $ fromJust rpnl
    when (isJust ab) $
        atomically $
        writeTVar availableBalance $ fromJust ab
    when (isJust wb) $
        atomically $ writeTVar walletBalance $ fromJust wb
processResponse (Just execResp@(Exe TABLE {_data = execData})) BotState {..} _ =
    case execData V.!? 0 of
        Nothing -> return ()
        Just RespExecution {triggered = text} ->
            when (text == Just "StopOrderTriggered") $
            atomically $
            writeTBQueue
                (unSLWQueue slwQueue)
                (Just execResp)
processResponse _ _ _ = return ()

readResponse :: TBQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTBQueue q
    case r of
        Nothing -> retry
        Just x  -> return x

updateVar :: (Eq a) => TVar a -> a -> STM ()
updateVar var newVal = do
    currVal <- readTVar var
    if currVal == newVal
        then return ()
        else writeTVar var newVal
