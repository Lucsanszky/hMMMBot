module Bot.Types
    ( BotState(..)
    ) where

import           BasicPrelude
import           BitMEXClient
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Network.WebSockets            (Connection)

data BotState = BotState
    { connection     :: !Connection
    , positionQueue  :: !(TQueue (Maybe Response))
    , lobQueue       :: !(TQueue (Maybe Response))
    , orderQueue     :: !(TQueue (Maybe Response))
    , marginQueue    :: !(TQueue (Maybe Response))
    , executionQueue :: !(TQueue (Maybe Response))
    , messageQueue   :: !(TQueue (Maybe Response))
    , positionsMap   :: !(TVar (HashMap Text (Double, Double)))
    , stopLossMap    :: !(TVar (HashMap Text Text))
    }
-- newtype BitMEXBot m a = BitMEXBot
--     { runBot :: (R.ReaderT BotState (BitMEXReader m) a)
--     } deriving ( Applicative
--                , Functor
--                , Monad
--                , MonadIO
--                , R.MonadReader BotState
--                )
