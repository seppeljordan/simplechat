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
             | CmdError String
             | Message String
             | PM String String
               deriving (Show)

parseOnly str x =
    string str >> eof >> return x

parseQuit =
    parseOnly "quit" Quit

parseNick =
    string "nick " >>
    many1 letter >>= \name ->
    eof >>
    return (Nick name)

parseWho =
    parseOnly "who" Who

parseHelp =
    parseOnly "help" Help

parsePM =
    string "pm" >>
    many1 space >>
    many1 letter >>= \target ->
    many1 space >>
    many1 anyChar >>= \msg ->
    eof >>
    return (PM target msg)

parseCommand =
    (many.char) ' ' >>
    char '/' >>
    (try parseQuit <|>
     try parseNick <|>
     try parseWho <|>
     try parseHelp <|>
     try parsePM <|>
     (many anyChar >> eof >> return (CmdError "Command not recognized") )
    )

parseMessage = many anyChar >>= \msg ->
               eof >>
               return (Message msg)

cmdLine = try parseCommand <|> parseMessage


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
    , "    /pm NAME MESSAGE - send privat MESSAGE to NAME"
    ]
