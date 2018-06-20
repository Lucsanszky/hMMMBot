module Bot.Concurrent
    ( processResponse
    , readResponse
    , riskManager
    ) where

import           BasicPrelude                  hiding (head)
import           BitMEXClient
import           Bot.Types
import           Control.Concurrent.STM.TQueue
import           Control.Monad.STM
import           Data.Vector                   (head, (!?))

riskManager ::
       TQueue (Maybe Response) -> STM (Double, Maybe Double)
riskManager positionQueue = do
    r <- readTQueue positionQueue
    case r of
        Nothing -> retry
        Just x ->
            case x of
                P (TABLE {_data = positionData}) -> do
                    let RespPosition { currentQty = qty
                                     , avgCostPrice = price
                                     } = head positionData
                    case qty of
                        Nothing -> retry
                        Just q  -> return (q, price)
                _ -> retry

processResponse :: BotState -> Maybe Response -> STM ()
processResponse (BotState {..}) msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 t ->
                    writeTQueue lobQueue (Just (OB10 t))
                P t ->
                    writeTQueue positionQueue (Just (P t))
                O t -> writeTQueue orderQueue (Just (O t))
                M t -> writeTQueue marginQueue (Just (M t))
                Exe t ->
                    writeTQueue
                        executionQueue
                        (Just (Exe t))
                x -> writeTQueue messageQueue (Just x)

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x
