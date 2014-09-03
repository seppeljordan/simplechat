#!/usr/bin/runhaskell
module Main (main)

where

import Control.Concurrent hiding (isEmptyChan)
import Data.List
import Data.Time.Clock
import Data.Time.Format
import qualified Network.Socket as Net
import System.IO
import System.Locale

import Chat.Client
import Chat.Cmd
import Network.Listener


-- |Standard port
port :: Net.PortNumber
-- We have to use this strange syntax because otherwise the wrong port
-- will be assigned
port = fromIntegral (4141 :: Int)


-- |Adresses that are allowed to connect
host :: Net.HostAddress
host = Net.iNADDR_ANY


-- |Open a socket and listen on the socket
openSocket :: IO Net.Socket
openSocket =
    Net.socket Net.AF_INET Net.Stream Net.defaultProtocol >>= \s ->
    Net.setSocketOption s Net.ReuseAddr 1 >>
    Net.bind s (Net.SockAddrInet port host) >>
    Net.listen s 2 >>
    logMsg ("Socket opened on port "++show port) >>
    return s


findNewName :: [String] -> String
findNewName strs =
    let iter n s = if not ("NoName"++show (n::Int) `elem` s)
                   then "NoName"++show n
                   else iter (n+1) s
    in iter 0 (filter (isPrefixOf "NoName") strs)


findNewNames :: Int -> [String] -> [String]
findNewNames 0 _ = []
findNewNames n strs =
    let strs' = (findNewName strs):strs
    in (findNewNames (n-1) strs') ++ strs'


-- |Make a function that takes a string and returns a client from a socket
getClientConstructor :: Net.Socket -> IO (String -> ChatClient)
getClientConstructor sock =
    logMsg "Client tries to connect" >>
    socketToClient "NoName" sock >>= \cl ->
    return (\newName -> cl { clName=newName })


newtype Message = Message (UTCTime, String)


instance Show Message where
    show (Main.Message (t, s)) =
        let showTime = formatTime defaultTimeLocale "%d.%m, (%H:%M)" t
        in showTime ++ ": " ++ s


makeMsg :: String -> IO Message
makeMsg text =
    fmap (\time -> Main.Message (time,text)) getCurrentTime


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


sendMessage :: String -> ChatClient -> IO ()
sendMessage text cl =
    makeMsg text >>= \msg ->
    send (show msg) cl


mainLoop :: Listener (String -> ChatClient) -> [ChatClient] -> IO ()
mainLoop l clients =
    threadDelay 5000 >>
    listenChannel l >>=
    installNewConnections clients >>=
    checkClientsForInput >>=
    checkActions >>=
    mainLoop l


checkActions :: [ChatClient] -> IO [ChatClient]
checkActions = handleClients executeCmd


executeCmd :: Command -> ([ChatClient] -> ChatClient -> IO ChatClient)
executeCmd Quit =
    \cls cl -> quitConnection cls cl
executeCmd (Chat.Cmd.Message s) =
    \cls cl -> let others = filter (not.(== cl)) cls
               in broadcastMessage others (clName cl++": "++s) >>
                  return cl
executeCmd Help =
    \_ cl -> send commandHelp cl >>
               return cl
executeCmd Who =
    \cls cl -> send ((unwords.map clName) cls) cl >>
               return cl
executeCmd (Nick newName) =
    \cls cl -> if not (newName `elem` (map clName cls))
               then let oldName = clName cl
                    in broadcastMessage cls (oldName ++ " -> "++newName) >>
                       return cl { clName = newName }
               else sendMessage (newName++" is already taken") cl >>
                    return cl
executeCmd (CmdError msg) =
    \_ cl -> send msg cl >>
             send commandHelp cl >>
             return cl
executeCmd (PM target msg) =
    \cls cl -> sendPM cls cl target msg >>
               return cl


sendPM :: [ChatClient] -> ChatClient -> String -> String -> IO ()
sendPM cls src destName _
    | (null.filter (\x -> clName x == destName)) cls =
        sendMessage ("User \'"++destName++"\' not found") src
sendPM cls src destName text =
    let targets = filter (\x -> clName x == destName) cls
        msg = "<"++clName src++"> "++text
    in (sequence_.map (sendMessage msg)) targets


quitConnection :: [ChatClient] -> ChatClient -> IO ChatClient
quitConnection cls cl =
    let others = filter (not.(== cl)) cls
    in send "Disconnecting..." cl >>
       broadcastMessage others (clName cl++" disconnected") >>
       disconnectClient cl



installNewConnections :: [ChatClient] -> [String -> ChatClient] -> IO [ChatClient]
installNewConnections oldCls newCls =
    let names = map clName oldCls
        namedCls = zipWith (\n cl -> cl n) newNames newCls
        newNames = findNewNames (length newCls) names
    in (sequence_.map (greetNewClient oldCls)) namedCls >>
       return (namedCls ++ oldCls)


greetNewClient :: [ChatClient] -> ChatClient -> IO ()
greetNewClient oldCls newCl =
    let names = (unwords . map clName) oldCls
        msg = unlines
              [ "Welcome "++show (clName newCl)++".  Currently in the chat are:"
              , names
              , commandHelp
              , "Happy chatting."
              ]
    in send msg newCl >> logMsg ("Greeting client "++clName newCl)


logMsg :: String -> IO ()
logMsg = putStrLn


main :: IO ()
main =
    openSocket >>=
    spawnListener getClientConstructor >>= \l ->
    mainLoop l [] >>
    killListener l
