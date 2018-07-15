module Bot.Concurrent
    (
     readResponse
    , updateVar
    -- , waitForPriceChange
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
