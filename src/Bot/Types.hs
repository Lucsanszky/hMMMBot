module Bot.Types
    ( BotState(..)
    , BitMEXBot(..)
    , StopPx(..)
    , LimitPx(..)
    , Qty(..)
    , Rule(..)
    , ClientID(..)
    , LinkID(..)
    , OrderID(..)
    , PositionType(..)
    , PnLQueue(..)
    , RiskManagerQueue(..)
    , StopLossWatcherQueue(..)
    ) where

import           BasicPrelude
import           BitMEX                         as Mex
    ( Leverage (..)
    )
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
import           Data.IORef                     (IORef)
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

newtype RiskManagerQueue = RiskManagerQueue
    { unRiskManagerQueue :: TBQueue (Maybe Response)
    }

newtype StopLossWatcherQueue = StopLossWatcherQueue
    { unSLWQueue :: TBQueue (Maybe Response)
    }

newtype PnLQueue = PnLQueue
    { unPnlQueue :: TBQueue (Maybe Response)
    }

data PositionType
    = Long
    | Short
    | None
    deriving (Eq, Show)

data Rule
    = XBt_to_XBT
    | XBT_to_XBt
    | USD_to_XBT
    | XBT_to_USD

data BotState = BotState
    { connection       :: !Connection
    , riskManagerQueue :: !RiskManagerQueue
    , slwQueue         :: !StopLossWatcherQueue
    , pnlQueue         :: !PnLQueue
    , realPnl          :: !(TVar Integer)
    , prevBalance      :: !(TVar Integer)
    , availableBalance :: !(TVar Integer)
    , walletBalance    :: !(TVar Integer)
    , prevAsk          :: !(IORef Double)
    , prevBid          :: !(IORef Double)
    , prevPosition     :: !(TVar PositionType)
    , positionSize     :: !(IORef Integer)
    , openBuys         :: !(IORef Integer)
    , openSells        :: !(IORef Integer)
    , openBuyCost      :: !(IORef Integer)
    , openSellCost     :: !(IORef Integer)
    , buyID            :: !(IORef OrderID)
    , sellID           :: !(IORef OrderID)
    , stopOrderId      :: !(TVar OrderID)
    , leverage         :: !Mex.Leverage
    }

newtype BitMEXBot a = BitMEXBot
    { runBot :: (ReaderT BotState BitMEXReader) a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BotState
               )
