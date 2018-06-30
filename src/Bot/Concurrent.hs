module Bot.Concurrent
    ( processResponse
    , readResponse
    ) where

import           BasicPrelude                  hiding (head)
import           BitMEXClient
    ( RespExecution (..)
    , RespMargin (..)
    , RespPosition (..)
    , Response (..)
    , TABLE (..)
    )
import           Bot.Types
import           Control.Concurrent.STM.TQueue
    ( TQueue
    , readTQueue
    , writeTQueue
    )
import           Control.Monad.STM             (STM, retry)
import           Data.Vector                   (head, (!?))

processResponse :: BotState -> Maybe Response -> STM ()
processResponse (BotState {..}) msg = do
    case msg of
        Nothing -> return ()
        Just r ->
            case r of
                OB10 t ->
                    writeTQueue
                        (unLobQueue lobQueue)
                        (Just (OB10 t))
                posResp@(P (TABLE {_data = positionData})) -> do
                    let RespPosition {currentQty = currQty} =
                            head positionData
                    case currQty of
                        Nothing -> return ()
                        Just _ ->
                            writeTQueue
                                (unRiskManagerQueue
                                     riskManagerQueue)
                                (Just posResp)
                marginResp@(M (TABLE {_data = marginData})) -> do
                    let RespMargin {realisedPnl = rpnl} =
                            head marginData
                    case rpnl of
                        Nothing -> return ()
                        Just _ ->
                            writeTQueue
                                (unPnlQueue pnlQueue)
                                (Just marginResp)
                execResp@(Exe (TABLE {_data = execData})) -> do
                    case execData !? 0 of
                        Nothing -> return ()
                        Just (RespExecution {triggered = text}) ->
                            case text of
                                Just "StopOrderTriggered" ->
                                    writeTQueue
                                        (unSLWQueue slwQueue)
                                        (Just execResp)
                                _ -> return ()
                _ -> return ()

readResponse :: TQueue (Maybe Response) -> STM Response
readResponse q = do
    r <- readTQueue q
    case r of
        Nothing -> retry
        Just x  -> return x
