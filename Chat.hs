#!/usr/bin/runhaskell
module Chat (main)

where

import Control.Concurrent hiding (isEmptyChan)
import Control.Exception
import Data.Time.Clock
import Data.Time.Format
import qualified Network.Socket as Net
import System.IO
import System.Locale

import Chat.Cmd
import Network.Listener


port :: Net.PortNumber
port = Net.PortNum 4141


host :: Net.HostAddress
host = Net.iNADDR_ANY


openSocket :: IO Net.Socket
openSocket =
    Net.socket Net.AF_INET Net.Stream Net.defaultProtocol >>= \s ->
    Net.setSocketOption s Net.ReuseAddr 1 >>
    Net.bind s (Net.SockAddrInet port host) >>
    Net.listen s 2 >>
    logMsg ("Socket opened on port "++show port) >>
    return s


clientFromSocket :: Net.Socket -> IO ChatClient
clientFromSocket sock =
    logMsg "Client tries to connect" >>
    socketToClient "NoName" sock


newtype Message = Message (UTCTime, String)


instance Show Message where
    show (Chat.Message (t, s)) =
        let showTime = formatTime defaultTimeLocale "%d.%m, (%H:%M)" t
        in showTime ++ ": " ++ s


makeMsg :: String -> IO Message
makeMsg text =
    fmap (\time -> Chat.Message (time,text)) getCurrentTime


data ChatClient = ChatClient { clName :: String
                             , clHandle :: Handle
                             , clBuffer :: [Command]
                             , clConnected :: Bool
                             }


instance Eq ChatClient where
    (==) cl1 cl2 =
        clHandle cl1 == clHandle cl2


addLineToBuffer :: String -> ChatClient -> ChatClient
addLineToBuffer [] cl = cl
addLineToBuffer line cl | words line == [] = cl
                        | otherwise =
                            cl { clBuffer = (clBuffer cl) ++ [parseCmd (init line)] }


emptyBuffer :: ChatClient -> ChatClient
emptyBuffer cl =
    cl { clBuffer = [] }


getMsg :: ChatClient -> (ChatClient, Maybe Command)
getMsg cl | hasMsg cl = ( cl { clBuffer = (tail.clBuffer) cl }
                        , Just ((head.clBuffer) cl)
                        )
          | otherwise = (cl, Nothing)


hasMsg :: ChatClient -> Bool
hasMsg cl | (not.null.clBuffer) cl = True
          | otherwise = False


disconnectClient :: ChatClient -> ChatClient
disconnectClient cl =
    cl { clConnected = False }


socketToClient :: String -> Net.Socket -> IO ChatClient
socketToClient name socket =
    let construct hdl = ChatClient { clName = name
                                   , clHandle = hdl
                                   , clBuffer = []
                                   , clConnected = True
                                   }
    in fmap construct (Net.socketToHandle socket ReadWriteMode)


broadcast :: [ChatClient] -> String -> IO ()
broadcast cls str =
    (sequence_ . map (send str)) cls >>
    logMsg ("Broadcasted: "++str)


broadcastMessage :: [ChatClient] -> String -> IO ()
broadcastMessage cls text =
    makeMsg text >>= \msg ->
    broadcast cls (show msg)


send :: String -> ChatClient -> IO ()
send str cl =
    if clConnected cl
    then hPutStrLn (clHandle cl) str >> hFlush (clHandle cl)
    else return ()


mainLoop :: Listener ChatClient -> [ChatClient] -> IO ()
mainLoop l clients =
    threadDelay 5000 >>
    listenChannel l >>=
    installNewConnections clients >>=
    checkClientsForInput >>=
    checkActions >>=
    mainLoop l


checkActions :: [ChatClient] -> IO [ChatClient]
checkActions cls =
    let iter [] done = return done
        iter (x:pending) done = handleClient x ([x]++pending++done) >>= \x' ->
                                   iter pending (x':done)
    in iter cls [] >>= \cls' ->
       return (filter clConnected cls')


handleClient :: ChatClient -> [ChatClient] -> IO ChatClient
handleClient cl cls =
    let (cl', mcmd) = getMsg cl
    in maybe
       (return cl')
       (\cmd -> (executeCmd cmd) cl' cls >>= \cl'' ->
                handleClient cl'' cls)
       mcmd


executeCmd :: Command -> (ChatClient -> [ChatClient] -> IO ChatClient)
executeCmd Quit =
    \cl cls -> quitConnection cls cl >>
               return ((disconnectClient.emptyBuffer) cl)
executeCmd (Chat.Cmd.Message s) =
    \cl cls -> let others = filter (not.(== cl)) cls
               in broadcastMessage others (clName cl++": "++s) >>
                  return cl
executeCmd Help =
    \cl _ -> send commandHelp cl >>
               return cl
executeCmd Who =
    \cl cls -> send ((unwords.map clName) cls) cl >>
               return cl
executeCmd (Nick newName) =
    \cl cls -> let oldName = clName cl
               in broadcastMessage cls (oldName ++ " -> "++newName) >>
                  return cl { clName = newName }
executeCmd (CmdError msg) =
    \cl _ -> send msg cl >>
             send commandHelp cl >>
             return cl


quitConnection :: [ChatClient] -> ChatClient -> IO [ChatClient]
quitConnection cls cl =
    let others = filter (not.(== cl)) cls
    in send "Disconnecting..." cl >>
       broadcastMessage others (clName cl++" disconnected") >>
       hClose (clHandle cl)
       >> return others


installNewConnections :: [ChatClient] -> [ChatClient] -> IO [ChatClient]
installNewConnections oldCls newCls =
    (sequence_.map (greetNewClient oldCls)) newCls >>
    return (newCls ++ oldCls)


greetNewClient :: [ChatClient] -> ChatClient -> IO ()
greetNewClient oldCls newCl =
    let names = (unwords . map clName) oldCls
        msg = unlines
              [ "Welcome.  Currently in the chat are:"
              , names
              , commandHelp
              , "Happy chatting."
              ]
    in send msg newCl >> logMsg ("Greeting client "++clName newCl)


checkClientsForInput :: [ChatClient] -> IO [ChatClient]
checkClientsForInput cls =
    (sequence.map checkClientForInput) cls


checkClientForInput :: ChatClient -> IO ChatClient
checkClientForInput cl =
    let hdl = (clHandle cl)
    in hReady hdl >>= \avail ->
       if avail
       then hGetLine hdl >>= \line ->
            (return (addLineToBuffer line cl))
       else return cl


logMsg :: String -> IO ()
logMsg = putStrLn


main :: IO ()
main =
    openSocket >>=
    spawnListener clientFromSocket >>= \l ->
    mainLoop l [] >>
    killListener l
