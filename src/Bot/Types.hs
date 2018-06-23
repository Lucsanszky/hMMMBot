module Bot.Types
    ( BotState(..)
    , BitMEXBot(..)
    ) where

import           BasicPrelude
import           BitMEXClient
    ( BitMEXReader
    , Response
    )
import           Control.Concurrent.STM.TQueue (TQueue)
import           Control.Concurrent.STM.TVar   (TVar)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT
    )
import           Network.WebSockets            (Connection)

data BotState = BotState
    { connection     :: !Connection
    , positionQueue  :: !(TQueue (Maybe Response))
    , lobQueue       :: !(TQueue (Maybe Response))
    , orderQueue     :: !(TQueue (Maybe Response))
    , marginQueue    :: !(TQueue (Maybe Response))
    , executionQueue :: !(TQueue (Maybe Response))
    , messageQueue   :: !(TQueue (Maybe Response))
    , positionSize   :: !(TVar Int)
    , stopLossMap    :: !(TVar (HashMap Text Text))
    }

newtype BitMEXBot m a = BitMEXBot
    { runBot :: (ReaderT BotState (BitMEXReader m) a)
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BotState
               )
