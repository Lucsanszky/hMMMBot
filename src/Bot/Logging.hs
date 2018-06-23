module Bot.Logging
    ( esLoggingContext
    , withEsLoggingWS
    , initEsLogContext
    ) where

import           BasicPrelude
import           BitMEX.Logging
import           BitMEXClient
import           Database.V5.Bloodhound
import           Katip
    ( ColorStrategy (ColorIfTerminal)
    , Severity (DebugS)
    , Verbosity (V3)
    , defaultScribeSettings
    , initLogEnv
    , mkHandleScribe
    , registerScribe
    )
import           Katip.Scribes.ElasticSearch
import           Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    )
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )

initEsLogContext :: IO LogContext
initEsLogContext = initLogEnv "hMMMBot" "dev"

withEsLoggingWS ::
       Text -> Text -> BitMEXWrapperConfig -> IO BitMEXWrapperConfig
withEsLoggingWS user pw p = do
    logCxt <- esLoggingContext (logContext p) user pw
    return $
        p
        { logExecContext = stdoutLoggingExec
        , logContext = logCxt
        }

esLoggingContext :: LogContext -> Text -> Text -> IO LogContext
esLoggingContext cxt user pw
 = do
    mgr <- newManager tlsManagerSettings
    let bhe = mkBHEnv (Server "https://61abc218de484ebd8e5b1cb984092716.eu-west-1.aws.found.io:9243") mgr
    esScribe <-
        mkEsScribe
            defaultEsScribeCfgV5
            bhe { bhRequestHook = basicAuthHook (EsUsername user) (EsPassword pw)}
            (IndexName "katip")
            (MappingName "hMMMBot-logs")
            DebugS
            V3
    registerScribe "es" esScribe defaultScribeSettings cxt
