module Bot.Types
    ( BotState(..)
    , BitMEXBot(..)
    , StopPx(..)
    , LimitPx(..)
    , Qty(..)
    , ClientID(..)
    , LinkID(..)
    , OrderID(..)
    , PositionQueue(..)
    , LOBQueue(..)
    , OrderQueue(..)
    , ExecutionQueue(..)
    , MarginQueue(..)
    , MessageQueue(..)
    , StopLossTriggered(..)
    , PnLQueue(..)
    , RiskManagerQueue(..)
    , StopLossWatcherQueue(..)
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

newtype StopPx =
    StopPx (Maybe Double)
    deriving (Eq, Ord, Show)

newtype LimitPx =
    LimitPx (Maybe Double)
    deriving (Eq, Ord, Show)

newtype Qty =
    Qty (Maybe Double)
    deriving (Eq, Ord, Show)

newtype ClientID =
    ClientID (Maybe Text)
    deriving (Eq, Ord, Show)

newtype LinkID =
    LinkID (Maybe Text)
    deriving (Eq, Ord, Show)

newtype OrderID =
    OrderID (Maybe Text)
    deriving (Eq, Ord, Show)

newtype PositionQueue = PositionQueue
    { unPositionQueue :: TQueue (Maybe Response)
    }

newtype LOBQueue = LOBQueue
    { unLobQueue :: TQueue (Maybe Response)
    }

newtype OrderQueue = OrderQueue
    { unOrderQueue :: (TQueue (Maybe Response))
    }

newtype MarginQueue = MarginQueue
    { unMarginQueue :: (TQueue (Maybe Response))
    }

newtype ExecutionQueue = ExecutionQueue
    { unExecutionQueue :: (TQueue (Maybe Response))
    }

newtype MessageQueue = MessageQueue
    { unMessageQueue :: (TQueue (Maybe Response))
    }

newtype RiskManagerQueue = RiskManagerQueue
    { unRiskManagerQueue :: TQueue (Maybe Response)
    }

newtype StopLossWatcherQueue = StopLossWatcherQueue
    { unSLWQueue :: (TQueue (Maybe Response))
    }

newtype PnLQueue = PnLQueue
    { unPnlQueue :: (TQueue (Maybe Response))
    }

data StopLossTriggered
    = Short
    | Long
    | None
    deriving (Eq, Show)

data BotState = BotState
    { connection :: !Connection
    , riskManagerQueue  :: !RiskManagerQueue
    , slwQueue          :: !StopLossWatcherQueue
    , lobQueue          :: !LOBQueue
    , pnlQueue          :: !PnLQueue
    , positionSize      :: !(TVar Int)
    , stopLossMap       :: !(TVar (HashMap Text (Text, Double)))
    , stopLossTriggered :: !(TVar StopLossTriggered)
    }

newtype BitMEXBot m a = BitMEXBot
    { runBot :: (ReaderT BotState (BitMEXReader m) a)
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BotState
               )
