{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage logLine =
    case words logLine of
    ("E" : sev : ts : rest) -> LogMessage (Error $ read sev) (read ts) $ unwords rest
    ("I" : ts : rest)       -> LogMessage Info (read ts) $ unwords rest
    ("W" : ts : rest)       -> LogMessage Warning (read ts) $ unwords rest
    _                       -> Unknown logLine


parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
