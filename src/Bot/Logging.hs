module Bot.Logging
    ( esLoggingContext
    , withEsLoggingWS
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
    , mkHandleScribe
    , registerScribe
    )
import           Katip.Scribes.ElasticSearch
import           Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    )

withEsLoggingWS ::
       BitMEXWrapperConfig -> IO BitMEXWrapperConfig
withEsLoggingWS p = do
    logCxt <- esLoggingContext (logContext p)
    return $
        p
        { logExecContext = stdoutLoggingExec
        , logContext = logCxt
        }

esLoggingContext :: LogContext -> IO LogContext
esLoggingContext cxt
    -- handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stdout LG.InfoS LG.V2
 = do
    mgr <- newManager defaultManagerSettings
    let bhe = mkBHEnv (Server "http://localhost:9200") mgr
    esScribe <-
        mkEsScribe
            defaultEsScribeCfgV5
            bhe
            (IndexName "all-indices-prefixed-with")
            (MappingName "application-logs")
            DebugS
            V3
    registerScribe "es" esScribe defaultScribeSettings cxt
