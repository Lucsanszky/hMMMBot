module Bot.Logging
    ( esLoggingContext
    , initEsLogContext
    ) where

import           BasicPrelude
import           BitMEX.Logging              (LogContext)
import           Database.V5.Bloodhound
    ( EsPassword (..)
    , EsUsername (..)
    , IndexName (..)
    , MappingName (..)
    , Server (..)
    , basicAuthHook
    , bhRequestHook
    , mkBHEnv
    )
import           Katip
    ( Severity (..)
    , Verbosity (..)
    , defaultScribeSettings
    , initLogEnv
    , registerScribe
    )
import           Katip.Scribes.ElasticSearch
    ( defaultEsScribeCfgV5
    , mkEsScribe
    )
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )

initEsLogContext :: IO LogContext
initEsLogContext = initLogEnv "hMMMBot" "dev"

esLoggingContext :: Text -> Text -> LogContext -> IO LogContext
esLoggingContext user pw cxt
 = do
    mgr <- newManager tlsManagerSettings
    let bhe = mkBHEnv (Server "https://61abc218de484ebd8e5b1cb984092716.eu-west-1.aws.found.io:9243/") mgr
    esScribe <-
        mkEsScribe
            defaultEsScribeCfgV5
            bhe { bhRequestHook = basicAuthHook (EsUsername user) (EsPassword pw)}
            (IndexName "trading-session")
            (MappingName "hMMMBot")
            DebugS
            V3
    registerScribe "es" esScribe defaultScribeSettings cxt
