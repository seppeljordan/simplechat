#!/usr/bin/runhaskell
module Chat (main)

where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Data.IP
import Data.Time.Clock
import Data.Time.Format
import qualified Network.Socket as Net
import System.IO
import System.Locale

import Network.Listener

port = Net.PortNum 4141
host = Net.iNADDR_ANY


filterEmpty :: (Eq a) => [[a]] -> [[a]]
filterEmpty xss =
    filter (\xs -> not (xs==[])) xss


openSocket :: IO Net.Socket
openSocket =
    Net.socket Net.AF_INET Net.Stream Net.defaultProtocol >>= \s ->
    Net.setSocketOption s Net.ReuseAddr 1 >>
    Net.bind s (Net.SockAddrInet port host) >>
    Net.listen s 2 >>
    logMsg ("Socket opened on port "++show port) >>
    return s


newtype Message = Message (UTCTime, String)


instance Show Message where
    show (Message (t, s)) =
        let showTime = formatTime defaultTimeLocale "%d.%m, (%H:%M)" t
        in showTime ++ ": " ++ s


makeMsg :: String -> IO Message
makeMsg text =
    fmap (\time -> Message (time,text)) getCurrentTime


readMessage :: IO Message
readMessage =
    getLine >>= \s ->
    getCurrentTime >>= \t ->
    return $ Message (t,s)


hPutMessage :: Handle -> Message -> IO ()
hPutMessage hdl m =
    hPutStrLn hdl (show m)


msgText :: Message -> String
msgText (Message (_,t)) = t


data ChatClient = ChatClient { clName :: String
                             , clHandle :: Handle
                             , clBuffer :: [String]
                             }


instance Eq ChatClient where
    (==) cl1 cl2 =
        clHandle cl1 == clHandle cl2


addLineToBuffer :: String -> ChatClient -> ChatClient
addLineToBuffer [] cl = cl
addLineToBuffer line cl | words line == [] = cl
                        | otherwise =
                            cl
                            { clBuffer = clBuffer cl ++ [line] }


getMsg :: ChatClient -> (ChatClient, Maybe String)
getMsg cl | hasMsg cl = ( cl { clBuffer = (filterEmpty.tail) (clBuffer cl) }
                        , Just ((head.filterEmpty.clBuffer) cl)
                        )
          | otherwise = (cl, Nothing)


hasMsg :: ChatClient -> Bool
hasMsg cl | clBuffer cl == [] = False
          | (filterEmpty.clBuffer) cl == []= False
          | (words.head.clBuffer) cl == [] = False
          | otherwise = True


socketToClient :: String -> Net.Socket -> IO ChatClient
socketToClient name socket =
    let construct hdl = ChatClient { clName = name
                                   , clHandle = hdl
                                   , clBuffer = []
                                   }
    in fmap construct (Net.socketToHandle socket ReadWriteMode)


fileClient :: String -> FilePath -> IO ChatClient
fileClient name path =
    let construct hdl = ChatClient { clName = name
                                   , clHandle = hdl
                                   , clBuffer = []
                                   }
    in fmap construct (openFile path ReadWriteMode)


closeClient :: ChatClient -> IO ()
closeClient cl =
    logMsg ("Connection to "++clName cl++" closed") >>
    hClose (clHandle cl)


broadcast :: [ChatClient] -> String -> IO ()
broadcast cls str =
    (sequence_ . map (send str)) cls >>
    logMsg ("Broadcasted: "++str)


mbroadcastMessage :: [ChatClient] -> Maybe String -> IO ()
mbroadcastMessage _ Nothing = return ()
mbroadcastMessage cls (Just text) =
    broadcastMessage cls text


broadcastMessage :: [ChatClient] -> String -> IO ()
broadcastMessage cls text =
    makeMsg text >>= \msg ->
    broadcast cls (show msg)


mbroadcast :: [ChatClient] -> Maybe String -> IO ()
mbroadcast _ Nothing = return ()
mbroadcast cls (Just msg) = broadcast cls msg


send :: String -> ChatClient -> IO ()
send str cl =
    hPutStrLn (clHandle cl) str >> hFlush (clHandle cl)


mainLoop :: Chan ChatClient -> [ChatClient] -> IO ()
mainLoop chan clients =
    threadDelay 1000 >>
    checkForConnections chan >>=
    installNewConnections clients >>=
    checkClientsForInput >>=
    disconnectClients >>=
    renameClients >>=
    broadcastAll >>=
    mainLoop chan


checkForConnections :: Chan ChatClient -> IO [ChatClient]
checkForConnections chan =
    isEmptyChan chan >>= \p ->
    if p
    then return []
    else readChan chan >>= \msg ->
         checkForConnections chan >>= \msgs ->
         return (msg:msgs)


broadcastAll :: [ChatClient] -> IO [ChatClient]
broadcastAll cls =
    (sequence.map (broadcastOne cls)) cls


broadcastOne :: [ChatClient] -> ChatClient -> IO ChatClient
broadcastOne cls cl =
    let others = filter (not.(== cl)) cls
        (newCl, mtext) = getMsg cl
    in return (fmap ((clName cl++": ")++) mtext) >>= \mmsg ->
       mbroadcastMessage others mmsg >>
       return newCl


disconnectClients :: [ChatClient] -> IO [ChatClient]
disconnectClients cls =
    let hasQuitCmd cl =
            maybe False
                  (\txt -> "quit" == (unwords.words) txt)
                  ((\(_,mtxt) -> mtxt) (getMsg cl))
        activeCls = filter (not.hasQuitCmd) cls
    in (sequence_.map (disconnectClient activeCls).filter hasQuitCmd) cls >>
       (return activeCls)


disconnectClient :: [ChatClient] -> ChatClient -> IO ()
disconnectClient cls cl =
    send "Disconnecting..." cl >>
    broadcastMessage cls (clName cl++" disconnected") >>
    hClose (clHandle cl)


renameClients :: [ChatClient] -> IO [ChatClient]
renameClients cls = (sequence.map (renameClient cls)) cls


hasRenameCommand :: ChatClient -> Bool
hasRenameCommand cl
    | (not.hasMsg) cl = False
    | otherwise =
        let (_,mtext) = getMsg cl
        in Just True == fmap (\txt ->
                                  assert ((length.words) txt /= 0) ((head.words) txt == "nick")
                             ) mtext


renameClient :: [ChatClient] -> ChatClient -> IO ChatClient
renameClient cls cl | (not.hasMsg) cl = return cl
                    | (not.hasRenameCommand) cl = return cl
renameClient cls cl =
    let (newCl, mtext) = getMsg cl
        oldName = clName cl
        makeMsg txt = getCurrentTime >>= \time -> return (show (Message (time, txt)))
    in if (head.words.(maybe "arg" id)) mtext == "nick"
       then let newName = (unwords.tail.words.(maybe "" id)) mtext
            in makeMsg (oldName ++ " -> " ++ newName) >>= \msg ->
                broadcast cls msg >>
                return (newCl { clName =  newName })
       else return cl


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
              , "Enter \'quit\' to leave the chat."
              , "Happy chatting."
              ]
    in send msg newCl >> logMsg ("Greeting client "++clName newCl)


checkClientsForInput :: [ChatClient] -> IO [ChatClient]
checkClientsForInput cls =
    (sequence.map checkClientForInput) cls


checkClientForInput :: ChatClient -> IO ChatClient
checkClientForInput cl =
    let hdl = (clHandle cl)
    in hReady (clHandle cl) >>= \avail ->
       if avail
       then hGetLine (clHandle cl) >>= \line ->
            return (addLineToBuffer line cl)
       else return cl


listner :: Net.Socket -> IO ChatClient
listner sock =
    logMsg "Client tries to connect" >>
    socketToClient "NoName" sock


logMsg :: String -> IO ()
logMsg = putStrLn


main :: IO ()
main =
    openSocket >>=
    spawnListener listner >>= \l ->
    mainLoop (getChan l) [] >>
    killListener l
