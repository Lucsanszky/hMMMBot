module Bot.Concurrent
    ( processResponse
    , readResponse
    ) where

import           BasicPrelude
import           BitMEXClient
    ( Response (..)
    )
import           Bot.Types
    ( BotState (..)
    )
import           Control.Concurrent.STM.TQueue
    ( TQueue
    , readTQueue
    , writeTQueue
    )
import           Control.Monad.STM             (STM, retry)

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
