module Main where

import           BasicPrelude            hiding (head)
-- import           BitMEX
import           BitMEXWebSockets
    ( Command (..)
    , RespOrderBook10 (..)
    , Response (..)
    , Symbol (..)
    , TABLE (..)
    , Topic (..)
    , sendMessage
    )
import           BitMEXWrapper
import           Control.Concurrent      (forkIO)
import qualified Control.Monad.Reader    as R (asks)
import           Data.Aeson
    ( Value (String)
    , decode
    , toJSON
    )
import qualified Data.ByteString         as B (readFile)
import           Data.ByteString.Char8   (pack)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Data.Vector             (head)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Network.WebSockets
    ( receiveData
    , sendClose
    , sendTextData
    )
import qualified System.Environment      as Env (getArgs)

tickSize = 0.5
strategyThreshold = 0.5
minPos = -2
maxPos = 2

-- check :: BitMEXApp ()
-- check = undefined

-- placeOrders :: BitMEXApp ()
-- placeOrders = undefined

-- aggressiveMM :: BitMEXApp ()
-- aggressiveMM = placeOrders

-- passiveMM :: BitMEXApp ()
-- passiveMM = placeOrders

-- runStrategy :: BitMEXApp ()
-- runStrategy = if imbalance > strategyThreshold
--     then aggressiveMM
--     else passiveMM

-- reset :: IO ()
-- reset = undefined

tradeLoop :: BitMEXApp ()
tradeLoop conn = do
    pub <- R.asks publicKey
    time <- liftIO $ makeTimestamp <$> getPOSIXTime
    sig <- sign (pack ("GET" ++ "/realtime" ++ show time))
    -- x <- makeRequest $ orderGetOrders (Accept MimeJSON)
    liftIO $ do
        -- print x
        _ <-
            forkIO $
            sendMessage
                conn
                AuthKey
                [ String pub
                , toJSON time
                , (toJSON . show) sig
                ]
        _ <-
            forkIO check

        _ <-
            forkIO $
            sendMessage
                conn
                Subscribe
                [OrderBook10 XBTUSD :: Topic Symbol]
        loop
        sendClose conn ("Connection closed" :: Text)
  where
    check = do
        msg <- receiveData conn
        case decode msg :: Maybe Response of
            Nothing -> print "Invalid response"
            Just r -> case r of
                Error x -> print x
                OB10 (TABLE {..}) -> do
                    let RespOrderBook10 {..} = head _data
                    print $ head asks
                    print $ head bids
                    check
                resp    -> do
                    print resp
                    check
    loop =
        getLine >>= \line ->
            -- unless (null (return line :: IO Text)) $
            sendTextData conn line >> loop



    -- whileM_ True $ do
    -- check
    -- runStrategy

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:_) <- Env.getArgs
    pub <- readFile pubPath
    priv <- B.readFile privPath
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            }
    connect config tradeLoop
