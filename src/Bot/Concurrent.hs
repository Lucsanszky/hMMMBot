module Bot.Concurrent
    ( processResponse
    , readResponse
    , updateVar
    ) where

import           BasicPrelude                   hiding
    ( head
    )
import           BitMEXClient
    ( RespExecution (..)
    , RespMargin (..)
    , RespOrderBook10 (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    )
import           Bot.Math
import           Bot.Types
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
import           Control.Monad.STM
    ( STM
    , atomically
    , retry
    )
import           Data.Vector                    (head, (!?))

processResponse :: BotState -> Maybe Response -> IO ()
processResponse (BotState {..}) msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 (TABLE {_data = orderBookData}) -> do
                    let RespOrderBook10 { asks = newAsks
                                        , bids = newBids
                                        } =
                            head orderBookData
                    atomically $ writeTVar obAsks newAsks
                    atomically $ writeTVar obBids newBids
                posResp@(P (TABLE {_data = positionData})) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderBuyCost = buyCost
                                     , openOrderSellQty = sellQty
                                     , openOrderSellCost = sellCost
                                     } = head positionData
                    when (currQty /= Nothing) $ do
                        let Just q = map floor currQty
                        atomically $
                            updateVar positionSize q
                        atomically $
                            writeTBQueue
                                (unRiskManagerQueue
                                     riskManagerQueue)
                                (Just posResp)
                    when (buyQty /= Nothing) $ do
                        let Just b = buyQty
                        atomically $ updateVar openBuys b
                    when (buyCost /= Nothing) $ do
                        let Just bc = buyCost
                        atomically $
                            updateVar openBuyCost bc
                    when (sellQty /= Nothing) $ do
                        let Just s = sellQty
                        atomically $ updateVar openSells s
                    when (sellCost /= Nothing) $ do
                        let Just sc = sellCost
                        atomically $
                            updateVar openSellCost sc
                    return ()
                marginResp@(M (TABLE {_data = marginData})) -> do
                    let RespMargin { realisedPnl = rpnl
                                   , availableMargin = ab
                                   , walletBalance = wb
                                   } = head marginData
                    when (rpnl /= Nothing) $ do
                        let Just p = rpnl
                        atomically $ writeTVar realPnl p
                    when (ab /= Nothing) $ do
                        let Just b = ab
                        atomically $
                            writeTVar availableBalance b
                    when (wb /= Nothing) $ do
                        let Just w = wb
                        atomically $
                            writeTVar walletBalance w
                execResp@(Exe (TABLE {_data = execData})) -> do
                    case execData !? 0 of
                        Nothing -> return ()
                        Just (RespExecution { triggered = text
                                            , ordStatus = stat
                                            }) -> do
                            when
                                (text ==
                                 Just "StopOrderTriggered" ||
                                 stat == Just "New") $
                                atomically $
                                writeTBQueue
                                    (unSLWQueue slwQueue)
                                    (Just execResp)
                            return ()
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
