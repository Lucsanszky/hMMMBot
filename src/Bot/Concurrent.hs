module Bot.Concurrent
    ( processResponse
    , readResponse
    ) where

import           BasicPrelude
import           BitMEXClient
import           Bot.Types
import           Control.Concurrent.STM.TQueue
import           Control.Monad.STM
import           Data.Vector                   (head, (!?))

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

waitTilProcessed :: BotState -> STM Bool
waitTilProcessed BotState {..} = do
    O (TABLE {_data = orderData}) <- readResponse orderQueue
    case orderData !? 0 of
        Nothing -> retry
        Just (RespOrder {clOrdID = id}) ->
            case id of
                Nothing -> retry
                Just clientId -> do
                    Exe (TABLE {_data = executionData}) <-
                        readResponse executionQueue
                    case executionData !? 0 of
                        Nothing -> retry
                        Just (RespExecution { clOrdID = respId
                                            , workingIndicator = working
                                            }) ->
                            case respId of
                                Nothing -> retry
                                Just x ->
                                    case (fmap
                                              (&& (x ==
                                                   clientId))
                                              working) of
                                        Just True ->
                                            return True
                                        _ -> retry
