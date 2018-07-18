module Bot.Concurrent
    ( readResponse
    , updateVar
    ) where

import           BasicPrelude
import           BitMEXClient
    ( Response (..)
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
import           Control.Monad.STM              (STM, retry)

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
