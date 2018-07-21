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
import           Data.IORef
    ( atomicWriteIORef
    )
import           Data.Maybe                     (fromJust)
import qualified Data.Vector                    as V
    ( head
    , (!?)
    )

processResponse ::
       BotState
    -> BitMEXWrapperConfig
    -> Maybe Response
    -> IO ()
processResponse botState@BotState {..} config msg =
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB TABLE {_action = Delete
                         , _data = orderBookData} -> do
                    trader botState config orderBookData
                OB10 TABLE {_data = orderBookData} -> do
                    let RespOrderBook10 { asks = newAsks
                                        , bids = newBids
                                        } =
                            V.head orderBookData
                    atomicWriteIORef bestAsk $ V.head $ V.head newAsks
                    atomicWriteIORef bestBid $ V.head $ V.head newBids
                posResp@(P TABLE {_data = positionData}) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderBuyCost = buyCost
                                     , openOrderSellQty = sellQty
                                     , openOrderSellCost = sellCost
                                     } = V.head positionData
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
                                   } = V.head marginData
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
                    case execData V.!? 0 of
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
