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

-- inserts a new `LogMessage` into an existing `MessageTree`,
-- producing a new `MessageTree`; the given `MessageTree` is presumed
-- to be sorted; the resulting `MessageTree` will also be a sorted
-- one, which means the time stamp of a `LogMessage` in any `Node`
-- will be greater than all time stamps of any `LogMessage` in the
-- left subtree and less than all time stamps of any `LobMessage in
-- the right subtree; if a `LogMessage` which is `Unknown` is given,
-- the given `MessageTree` is returned unchanged
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) a = a
insert a Leaf = Node Leaf a Leaf
insert a b = let (LogMessage _ d _) = a
                 (Node f g h) = b
                 (LogMessage _ j _) = g
              in if d < j
                 then (Node (insert a f) g h)
                 else (Node f g (insert a h))

-- builds up a `MessageTree` containing the `LogMessage`s in the
-- given list
build :: [LogMessage] -> MessageTree
build a = foldr insert Leaf a

-- takes a sorted `MessageTree` and produces a list of all the
-- `LogMessages` it contains, sorted by timestamp from smallest to
-- biggest
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node a b c) = (inOrder a) ++ [b] ++ (inOrder c)

-- takes an unsorted list of `LogMessages`, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or
-- greater, sorted by time stamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong a = [c | (LogMessage (Error b) _ c) <- (inOrder (build a)), b >= 50]
