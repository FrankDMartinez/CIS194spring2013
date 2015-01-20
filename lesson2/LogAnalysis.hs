{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parses a single string into a log message
parseMessage :: String -> LogMessage
parseMessage ('I':as) = let [(timeStamp,text)] = lex as
                         in LogMessage Info (read timeStamp) text
parseMessage ('W':as) = let [(timeStamp,text)] = lex as
                         in LogMessage Warning (read timeStamp) text
parseMessage ('E':as) = let [(severity,bs)] = lex as
                         in let [(timeStamp,text)] = lex bs
                          in LogMessage (Error (read severity)) (read timeStamp) text
parseMessage a = Unknown a

-- parses the contents from an entire file
parse :: String -> [LogMessage]
parse a = map parseMessage (lines a)
