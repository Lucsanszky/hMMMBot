module Main where

import           BasicPrelude            hiding
    ( getArgs
    , readFile
    )
import qualified BitMEX                  as Mex
import           BitMEXClient
import           Bot.Util
import qualified Control.Monad.Reader    as R
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Data.Aeson
import           Data.ByteString         (readFile)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment      (getArgs, withArgs)

main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:rest) <- getArgs
    pub <- T.readFile pubPath
    priv <- readFile privPath
    logCxt <- Mex.initLogContext
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    withArgs rest $
        defaultMainWith (defaultConfig { resamples = 9 })
            [ bench "post orders" $
              nfIO
                  (R.runReaderT
                       (run (do r <-
                                    updateLeverage
                                        XBTUSD
                                        (Mex.Leverage 3)
                                return ()))
                       config)
            ]
