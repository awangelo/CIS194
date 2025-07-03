{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

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
insert (Unknown _) tree              = tree
insert msg         Leaf              = Node Leaf msg Leaf
insert _ tree@(Node _ (Unknown _) _) = tree
insert msg@(LogMessage _ time1 _) (Node left msg2@(LogMessage _ time2 _) right)
    | time1 > time2                  = Node left msg2 (insert msg right)
    | otherwise                      = Node (insert msg left) msg2 right


build :: [LogMessage] -> MessageTree
build []         = Leaf
build (m : msgs) = insert m (build msgs)

-- ou `build = foldr insert Leaf`
--
-- foldr insert Leaf [m1, m2, m3]
-- = insert m1 (insert m2 (insert m3 Leaf))
--
--
-- Ou, expandindo:
--
-- Node (Node (Node Leaf m3 Leaf) m2 Leaf) m1 Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs =
  [msg | LogMessage (Error sev) _ msg <- inOrder (build msgs), sev >= 50]
