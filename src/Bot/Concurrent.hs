module Bot.Concurrent
    ( processResponse
    , readResponse
    ) where

import           BasicPrelude
import           BitMEXClient
    ( Response (..)
    )
import           Bot.Types
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
                    writeTQueue
                        (unLobQueue lobQueue)
                        (Just (OB10 t))
                P t ->
                    writeTQueue
                        (unPositionQueue positionQueue)
                        (Just (P t))
                O t ->
                    writeTQueue
                        (unOrderQueue orderQueue)
                        (Just (O t))
                M t ->
                    writeTQueue
                        (unMarginQueue marginQueue)
                        (Just (M t))
                Exe t ->
                    writeTQueue
                        (unExecutionQueue executionQueue)
                        (Just (Exe t))
                x ->
                    writeTQueue
                        (unMessageQueue messageQueue)
                        (Just x)

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x
