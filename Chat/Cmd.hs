module Chat.Cmd ( Command (..)
                , cmdToMstring
                , commandHelp
                , parseCmd
                )

where

import Text.Parsec


data Command = Quit
             | Nick String
             | Who
             | Help
             | Message String
               deriving (Show)

parseOnly str x =
    string str >> eof >> return x

parseQuit =
    parseOnly "/quit" Quit

parseNick =
    string "/nick " >> many letter >>= \name ->
    return (Nick name)

parseWho =
    parseOnly "/who" Who

parseHelp =
    parseOnly "/help" Help

parseMessage = many anyChar >>= \msg ->
               eof >>
               return (Message msg)

cmdLine =
    try parseQuit <|>
    try parseNick <|>
    try parseWho <|>
    try parseHelp <|>
    parseMessage


parseCmd :: String -> Command
parseCmd line =
    either (\_ -> error "parse error")
           id
           (parse cmdLine "(unknown)" line)


cmdToMstring :: Command -> Maybe String
cmdToMstring (Message s) = Just s
cmdToMstring _ = Nothing


commandHelp :: String
commandHelp =
    unlines
    [ "Commands:"
    , "    /quit - quit the chat"
    , "    /help - get this help message"
    , "    /nick STRING - change nickname to STRING"
    , "    /who - check who is logged in"
    ]
