module Bot.Concurrent
    ( processResponse
    , readResponse
    ) where

import           BasicPrelude                  hiding (head)
import           BitMEXClient
    ( RespExecution (..)
    , RespMargin (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    )
import           Bot.Types
import           Control.Concurrent.STM.TQueue
    ( TQueue
    , readTQueue
    , writeTQueue
    )
import           Control.Concurrent.STM.TVar   (writeTVar)
import           Control.Monad.STM
    ( STM
    , atomically
    , retry
    )
import           Data.Vector                   (head, (!?))

processResponse :: BotState -> Maybe Response -> IO ()
processResponse (BotState {..}) msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 t ->
                    atomically $
                    writeTQueue
                        (unLobQueue lobQueue)
                        (Just (OB10 t))
                posResp@(P (TABLE {_data = positionData})) -> do
                    let RespPosition { currentQty = currQty
                                     , openOrderBuyQty = buyQty
                                     , openOrderSellQty = sellQty
                                     } = head positionData
                    when (currQty /= Nothing) $ do
                        let Just q = map floor currQty
                        (atomically $ writeTVar positionSize q)
                        (atomically $
                         writeTQueue
                             (unRiskManagerQueue
                                  riskManagerQueue)
                             (Just posResp))
                    when (buyQty /= Nothing) $ do
                        let Just b = buyQty
                        (atomically $ writeTVar openBuys b)
                    when (sellQty /= Nothing) $ do
                        let Just s = sellQty
                        (atomically $ writeTVar openSells s)
                    return ()
                marginResp@(M (TABLE {_data = marginData})) -> do
                    let RespMargin {realisedPnl = rpnl} =
                            head marginData
                    case rpnl of
                        Nothing -> return ()
                        Just _ ->
                            atomically $
                            writeTQueue
                                (unPnlQueue pnlQueue)
                                (Just marginResp)
                execResp@(Exe (TABLE {_data = execData})) -> do
                    case execData !? 0 of
                        Nothing -> return ()
                        Just (RespExecution {triggered = text}) ->
                            case text of
                                Just "StopOrderTriggered" ->
                                    atomically $
                                    writeTQueue
                                        (unSLWQueue slwQueue)
                                        (Just execResp)
                                _ -> return ()
                _ -> return ()

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x
