module Bot
    ( initBot
    ) where

import           BasicPrelude                   hiding
    ( head
    )
import qualified BitMEX                         as Mex
import           BitMEXClient
import           Bot.Concurrent
import           Bot.OrderTemplates
import           Bot.RiskManager
import           Bot.Types
import           Bot.Util
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Monad.Reader           as R
    ( ask
    , asks
    , lift
    , runReaderT
    )
import           Control.Monad.STM
import           Data.Aeson
    ( Value (String)
    , toJSON
    )
import           Data.ByteString.Char8          (pack)
import           Data.Time.Clock.POSIX
    ( getPOSIXTime
    )
import           Data.Vector                    (head)
import           Network.HTTP.Client
    ( responseStatus
    )
import qualified Network.HTTP.Types.Status      as HTTP
    ( Status (..)
    )

_MAX_POSITION_ :: Integer
_MAX_POSITION_ = 42

_ORDER_SIZE_ :: Int
_ORDER_SIZE_ = 21

trade :: (Double, Double) -> BitMEXBot IO ()
trade (bestAsk, bestBid) = do
    BotState {..} <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
        newBestAsk = head $ head obAsks
        newBestBid = head $ head obBids
    size <- liftIO $ atomically $ readTVar positionSize
    case newBestAsk /= bestAsk || newBestBid /= bestBid of
        False -> do
            trade (bestAsk, bestBid)
        True -> do
            buys' <- liftIO $ atomically $ readTVar openBuys
            sells' <-
                liftIO $ atomically $ readTVar openSells
            let buys =
                    if size > 0
                        then size + buys'
                        else buys'
            let sells =
                    if size < 0
                        then (abs size) + sells'
                        else sells'
            if (buys < _MAX_POSITION_ ||
                sells < _MAX_POSITION_)
                then do
                    let (orders, newBuyQty, newSellQty) =
                            if (buys < _MAX_POSITION_ &&
                                sells < _MAX_POSITION_)
                                then ( [ limitSell
                                             newBestAsk
                                       , limitBuy newBestBid
                                       ]
                                     , buys + _MAX_POSITION_
                                     , sells +
                                       _MAX_POSITION_)
                                else if (buys <
                                         _MAX_POSITION_)
                                         then ( [ limitBuy
                                                      newBestBid
                                                ]
                                              , buys +
                                                _MAX_POSITION_
                                              , sells)
                                         else ( [ limitSell
                                                      newBestAsk
                                                ]
                                              , buys
                                              , sells +
                                                _MAX_POSITION_)
                    Mex.MimeResult {Mex.mimeResultResponse = resp} <-
                        makeMarket orders
                    let HTTP.Status {statusCode = code} =
                            responseStatus resp
                    if code == 200
                        then do
                            liftIO $
                                atomically $
                                updateVar openBuys newBuyQty
                            liftIO $
                                atomically $
                                updateVar
                                    openSells
                                    newSellQty
                            trade (newBestAsk, newBestBid)
                        else fail "order didn't go through"
                else do
                    trade (newBestAsk, newBestBid)

tradeLoop :: BitMEXBot IO ()
tradeLoop = do
    config <- BitMEXBot $ R.lift $ R.ask
    botState@(BotState {..}) <- R.ask
    OB10 (TABLE {_data = orderbookData}) <-
        liftIO $
        atomically $ readResponse $ unLobQueue lobQueue
    let RespOrderBook10 {asks = obAsks, bids = obBids} =
            head orderbookData
    _ <-
        liftIO $
        forkIO $ forever $ riskManager botState config
    _ <-
        liftIO $
        forkIO $ forever $ stopLossWatcher botState config
    _ <- liftIO $ forkIO $ forever $ pnlTracker pnlQueue
    trade (head $ head obAsks, head $ head obBids)

initBot :: BitMEXApp IO ()
initBot conn = do
    config <- R.ask
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    lobQueue <- liftIO $ atomically $ newTBQueue 1
    riskManagerQueue <- liftIO $ atomically $ newTBQueue 1
    slwQueue <- liftIO $ atomically $ newTBQueue 1
    pnlQueue <- liftIO $ atomically $ newTBQueue 1
    prevPosition <- liftIO $ atomically $ newTVar None
    positionSize <- liftIO $ atomically $ newTVar 0
    openBuys <- liftIO $ atomically $ newTVar 0
    openSells <- liftIO $ atomically $ newTVar 0
    stopOrderId <-
        liftIO $ atomically $ newTVar (OrderID Nothing)
    let botState =
            BotState
            { connection = conn
            , lobQueue = LOBQueue lobQueue
            , riskManagerQueue =
                  RiskManagerQueue riskManagerQueue
            , slwQueue = StopLossWatcherQueue slwQueue
            , pnlQueue = PnLQueue pnlQueue
            , prevPosition = prevPosition
            , positionSize = positionSize
            , openBuys = openBuys
            , openSells = openSells
            , stopOrderId = stopOrderId
            }
    liftIO $ do
        sendMessage
            conn
            AuthKey
            [String pub, toJSON time, (toJSON . show) sig]
        sendMessage
            conn
            Subscribe
            ([ OrderBook10 XBTUSD
             , Execution
             , Order
             , Position
             , Margin
             ] :: [Topic Symbol])
        forkIO $
            forever $ do
                msg <- getMessage conn config
                processResponse botState msg
    R.runReaderT (runBot tradeLoop) botState
