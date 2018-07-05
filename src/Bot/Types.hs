module Bot.Types
    ( BotState(..)
    , BitMEXBot(..)
    , StopPx(..)
    , LimitPx(..)
    , Qty(..)
    , Rule (..)
    , ClientID(..)
    , LinkID(..)
    , OrderID(..)
    -- , PositionQueue(..)
    , LOBQueue(..)
    -- , OrderQueue(..)
    , PositionType(..)
    -- , ExecutionQueue(..)
    -- , MarginQueue(..)
    -- , MessageQueue(..)
    , PnLQueue(..)
    , RiskManagerQueue(..)
    , StopLossWatcherQueue(..)
    ) where

import           BasicPrelude
import           BitMEXClient
    ( BitMEXReader
    , Response
    )
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Concurrent.STM.TVar    (TVar)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT
    )
import           Network.WebSockets             (Connection)

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

-- newtype PositionQueue = PositionQueue
--     { unPositionQueue :: TQueue (Maybe Response)
--     }

newtype LOBQueue = LOBQueue
    { unLobQueue :: TBQueue (Maybe Response)
    }

-- newtype OrderQueue = OrderQueue
--     { unOrderQueue :: (TQueue (Maybe Response))
--     }

-- newtype MarginQueue = MarginQueue
--     { unMarginQueue :: (TQueue (Maybe Response))
--     }

-- newtype ExecutionQueue = ExecutionQueue
--     { unExecutionQueue :: (TQueue (Maybe Response))
--     }

-- newtype MessageQueue = MessageQueue
--     { unMessageQueue :: (TQueue (Maybe Response))
--     }

newtype RiskManagerQueue = RiskManagerQueue
    { unRiskManagerQueue :: TBQueue (Maybe Response)
    }

newtype StopLossWatcherQueue = StopLossWatcherQueue
    { unSLWQueue :: (TBQueue (Maybe Response))
    }

newtype PnLQueue = PnLQueue
    { unPnlQueue :: (TBQueue (Maybe Response))
    }

data PositionType = Long | Short | None
    deriving (Eq, Show)

data Rule = XBt_to_XBT | USD_to_XBT

data BotState = BotState
    { connection       :: !Connection
    , riskManagerQueue :: !RiskManagerQueue
    , slwQueue         :: !StopLossWatcherQueue
    , lobQueue         :: !LOBQueue
    , pnlQueue         :: !PnLQueue
    , realPnl          :: !(TVar Integer)
    , startingBalance  :: !(TVar Integer)
    , availableBalance :: !(TVar Integer)
    , walletBalance    :: !(TVar Integer)
    , prevPosition     :: !(TVar PositionType)
    , positionSize     :: !(TVar Integer)
    , openBuys         :: !(TVar Integer)
    , openSells        :: !(TVar Integer)
    , stopOrderId      :: !(TVar OrderID)
    }

newtype BitMEXBot m a = BitMEXBot
    { runBot :: (ReaderT BotState (BitMEXReader m) a)
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BotState
               )
