module Network.Listener ( Listener
                        , spawnListener
                        , killListener
                        , getChan
                        , listenChannel
                        )

where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Network.Socket

type Listener a = (ThreadId, TChan a)

getChan :: Listener a -> TChan a
getChan (_,c) = c


checkSocket :: Socket -> IO ()
checkSocket sock =
    isListening sock >>= \p ->
    when (not p) (error "checkSocket: Socket is not listening")


makeListener :: (Socket -> IO a) -> Socket -> TChan a -> IO ()
makeListener f sock chan =
    accept sock >>= \(s,_) ->
    f s >>= \x ->
    atomically (writeTChan chan x) >>
    makeListener f sock chan


spawnListener :: (Socket -> IO a) -> Socket ->  IO (Listener a)
spawnListener f sock =
    checkSocket sock >>
    atomically newTChan >>= \chan ->
    forkIO (makeListener f sock chan) >>= \threadid ->
    return (threadid, chan)

killListener :: Listener a -> IO ()
killListener (threadId, _) =
    killThread threadId


listenChannel :: (Listener a) -> IO [a]
listenChannel l@(_,chan) =
    atomically (isEmptyTChan chan) >>= \p ->
    if p
    then return []
    else atomically (readTChan chan) >>= \msg ->
         listenChannel l >>= \msgs ->
         return (msg:msgs)
