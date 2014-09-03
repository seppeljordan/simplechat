module Chat.Client ( ChatClient (..)
                   , addLineToBuffer
                   , hasMsg
                   , getMsg
                   , disconnectClient
                   , checkClientsForInput
                   , handleClients
                   )

where

import System.IO

import Chat.Cmd


-- |Models a chat client connected to the chat server.
data ChatClient = ChatClient { clName :: String
                             , clHandle :: Handle
                             , clBuffer :: [Command]
                             , clConnected :: Bool
                             }


-- |If the handle of two clients is the same then the clients are the
-- same.
instance Eq ChatClient where
    (==) cl1 cl2 =
        clHandle cl1 == clHandle cl2


-- |Adds new command to the command buffer of a client and returns the
-- new client.
addLineToBuffer :: String -> ChatClient -> ChatClient
addLineToBuffer [] cl = cl
addLineToBuffer line cl | words line == [] = cl
                        | otherwise =
                            cl { clBuffer = (clBuffer cl) ++ [parseCmd (init line)] }


-- |Returns the the client given as an argument and returns the exact
-- same client except that the new client has an empty command buffer.
emptyBuffer :: ChatClient -> ChatClient
emptyBuffer cl =
    cl { clBuffer = [] }


-- |Returns a command if there is any in the buffer.
getMsg :: ChatClient -> (ChatClient, Maybe Command)
getMsg cl | hasMsg cl = ( cl { clBuffer = (tail.clBuffer) cl }
                        , Just ((head.clBuffer) cl)
                        )
          | otherwise = (cl, Nothing)


-- |Returns true if there are pending operations in the buffer of the
-- client
hasMsg :: ChatClient -> Bool
hasMsg cl | (not.null.clBuffer) cl = True
          | otherwise = False


-- |Flags a client as disconnected
disconnectClient :: ChatClient -> IO ChatClient
disconnectClient cl =
    hClose (clHandle cl) >>
    return (emptyBuffer $ cl { clConnected = False })


-- |Checks all the given clients for new input and returns a list of
-- clients where the input given is written into the clients command
-- buffer.
checkClientsForInput :: [ChatClient] -> IO [ChatClient]
checkClientsForInput cls =
    (sequence.map checkClientForInput) cls


-- |Check if there is any new input from the client to read and return
-- a new client with possibly new commands in the command buffer.
checkClientForInput :: ChatClient -> IO ChatClient
checkClientForInput cl =
    let hdl = (clHandle cl)
    in hReady hdl >>= \avail ->
       if avail
       then hGetLine hdl >>= \line ->
            (return (addLineToBuffer line cl))
       else return cl


-- |Check a list clients for pending commands and execute the commands
-- according to the given procedure.
handleClients :: (Command -> ([ChatClient] -> ChatClient -> IO ChatClient)) ->
                 [ChatClient] -> IO [ChatClient]
handleClients cmds cls =
    let iter [] done = return done
        iter (x:pending) done = handleClient cmds ([x]++pending++done) x >>= \x' ->
                                   iter pending (x':done)
    in iter cls [] >>= \cls' ->
       return (filter clConnected cls')


-- |Check a client for pending commands and execute the commands
-- according to the given input.
handleClient :: (Command -> ([ChatClient] -> ChatClient -> IO ChatClient)) ->
                [ChatClient] -> ChatClient -> IO ChatClient
handleClient handler cls cl =
    let (cl', mcmd) = getMsg cl
    in maybe
       (return cl')
       (\cmd -> (handler cmd) cls cl' >>=
                handleClient handler cls)
       mcmd
