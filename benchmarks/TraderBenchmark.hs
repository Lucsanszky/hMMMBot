module Main where

import           BasicPrelude             hiding
    ( getArgs
    , readFile
    )
import qualified BitMEX                   as Mex
    ( Leverage (..)
    , initLogContext
    , runDefaultLogExecWithContext
    )
import           BitMEXClient
    ( BitMEXReader (..)
    , BitMEXWrapperConfig (..)
    , Environment (..)
    , Symbol (..)
    , Topic (..)
    , getMessage
    , withConnectAndSubscribe
    )
import           Bot.Util                 (initBotState)
import           Control.Concurrent.Async
    ( async
    , waitAnyCatch
    )
import qualified Control.Monad.Reader     as R
    ( ask
    , runReaderT
    )
import           Criterion.Main
import           Criterion.Types
import           Data.ByteString          (readFile)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Time.Clock          (getCurrentTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.WebSockets       (receiveData)
import           System.Environment
    ( getArgs
    , withArgs
    )

botBenchmarks :: Mex.Leverage -> BitMEXReader ()
botBenchmarks lev = do
    config@BitMEXWrapperConfig {..} <- R.ask
    liftIO $
        withConnectAndSubscribe config [OrderBook10 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 60})
                [ bench "getMessage with OB10" $
                  nfIO $ do
                      msg <- receiveData c
                      getCurrentTime >>= print
                      print (msg :: T.Text)
                      return ()
                ]
    liftIO $
        withConnectAndSubscribe
            config
            [OrderBook10 XBTUSD, OrderBookL2 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OB10 and OBL2" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    ob10 <-
        liftIO $
        async $
        withConnectAndSubscribe config [OrderBook10 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OB10 inside async" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    obl2 <-
        liftIO $
        async $
        withConnectAndSubscribe config [OrderBookL2 XBTUSD] $ \c ->
            withArgs [] $
            defaultMainWith
                (defaultConfig
                 {resamples = 1000, timeLimit = 30})
                [ bench "getMessage with OBL2 inside async" $
                  nfIO $ do
                      msg <- getMessage c config
                      return ()
                ]
    eres <- liftIO $ waitAnyCatch [ob10, obl2]
    case eres of
        (_, Right _) -> return ()
        (_, Left _)  -> return ()

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:esUserPath:esPasswordPath:rest) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    user <- T.readFile esUserPath
    pw <- T.readFile esPasswordPath
    --logCxt <- initEsLogContext >>= esLoggingContext user pw
    logCxt <- Mex.initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = MainNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    R.runReaderT (run (botBenchmarks (Mex.Leverage 3))) config
