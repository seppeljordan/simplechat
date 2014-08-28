module Network.Listener ( Listener
                        , spawnListener
                        , killListener
                        , getChan
                        )

where

import Control.Concurrent.Chan
import Control.Concurrent
import Network.Socket

type Listener a = (ThreadId, Chan a)

getChan :: Listener a -> Chan a
getChan (_,c) = c

checkSocket :: Socket -> IO ()
checkSocket sock =
    isListening sock >>= \p ->
    if p
    then return ()
    else error "checkSocket: Socket is not listening"


makeListener :: (Socket -> IO a) -> Socket -> Chan a -> IO ()
makeListener f sock chan =
    accept sock >>= \(s,_) ->
    f s >>= \x ->
    writeChan chan x >>
    makeListener f sock chan


spawnListener :: (Socket -> IO a) -> Socket ->  IO (Listener a)
spawnListener f sock =
    isListening sock >>= \p ->
    (if p
     then return ()
     else error "spawnListener: Socket is not Listening") >>
    newChan >>= \chan ->
    forkIO (makeListener f sock chan) >>= \threadid ->
    return (threadid, chan)

killListener :: Listener a -> IO ()
killListener (threadId, _) =
    killThread threadId
