module Chat.Cmd ( Command (..)
                , cmdToMstring
                , commandHelp
                )

where

import Text.Read


data Command = Quit
             | Nick String
             | Who
             | Help
             | Message String
               deriving (Show)


instance Read Command where
    readsPrec _ s =
        let [(x,rest)] = lex s
        in case x of
             "quit" -> [(Quit, rest)]
             "nick" -> [( Nick ((fst.head.lex) rest)
                        , (snd.head.lex) rest
                        )]
             "help" -> [(Help,rest)]
             "who" -> [(Who, rest)]
             _ -> [(Message s, "")]


cmdToMstring :: Command -> Maybe String
cmdToMstring (Message s) = Just s
cmdToMstring _ = Nothing


commandHelp :: String
commandHelp =
    unlines
    [ "Commands:"
    , "    quit - quit the chat"
    , "    help - get this help message"
    , "    nick STRING - change nickname to STRING"
    , "    who - check who is logged in"
    ]
