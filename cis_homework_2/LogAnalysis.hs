{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('E':' ':ms) = createErrorFromLog ms
parseMessage ('I':' ':ms) = createInfoFromLog ms
parseMessage ('W':' ':ms) = createWarningFromLog ms
parseMessage x = Unknown x

createWarningFromLog :: String -> LogMessage
createWarningFromLog x = LogMessage Warning timestamp content
  where timestamp = (logTimeStamp (createLogParams x))
        content = (logContent (createLogParams x))

createInfoFromLog :: String -> LogMessage
createInfoFromLog x = LogMessage Info timestamp content
  where timestamp = (logTimeStamp (createLogParams x))
        content = (logContent (createLogParams x))

createErrorFromLog :: String -> LogMessage
createErrorFromLog x = LogMessage (Error code) timestamp content
  where code = (errorCode (createErrorParams x))
        timestamp = (errorTimeStamp (createErrorParams x))
        content = (errorContent (createErrorParams x))

createLogParams :: String -> (TimeStamp, String)
createLogParams x = (wordToInteger (head(words x)), unwords(drop 1(words x)))

logTimeStamp :: (TimeStamp, String) -> TimeStamp
logTimeStamp (timestamp, _) = timestamp
logContent :: (TimeStamp, String) -> String
logContent (_, content) = content

createErrorParams :: String -> (Int, TimeStamp, String)
createErrorParams x = (wordToInteger (head(words x)), wordToInteger (head(drop 1 (words x))), unwords(drop 2(words x)))

errorParams :: (String, String, String) -> (Int, TimeStamp, String)
errorParams (a,b,c) = (wordToInteger a,  wordToInteger b, c)

errorCode :: (Int, TimeStamp, String) -> Int
errorCode (code, _, _) = code
errorTimeStamp :: (Int, TimeStamp, String) -> TimeStamp
errorTimeStamp (_, timestamp, _) = timestamp
errorContent :: (Int, TimeStamp, String) -> String
errorContent (_, _, content) = content

wordToInteger :: String -> Int
wordToInteger n = read n :: Int

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- other ways of writing parse

--parse = (map parseMessage) . lines
-- I don't know why this doesn't need parse s
  -- maybe you can lave out the input & it's assumed?
-- (map parseMessage) needs a []
-- lines provides the []
--
-- 

-- parse s = map parseMessage $ lines s
-- map (function) list -> list
-- lines s resolves to list
-- the $ means that lines s resolves first.

theTimestamp :: LogMessage -> TimeStamp
theTimestamp (LogMessage _ t _) = t
theTimestamp (Unknown _ ) = 0

nodeMessage :: MessageTree -> LogMessage
nodeMessage (Node _ l _) = l
nodeMessage _ = Unknown ""

leftNode :: MessageTree -> MessageTree
leftNode (Node left _ _ ) = left

rightNode :: MessageTree -> MessageTree
rightNode (Node _ _ right ) = right

-- This was my first pass at the function, which was wrong.
-- insert :: LogMessage -> MessageTree -> MessageTree
-- insert (Unknown _) tree = tree
-- insert logMessage (Leaf) = Node Leaf logMessage Leaf
-- insert logMessage node
  -- | (theTimestamp logMessage) < (theTimestamp (nodeMessage node)) = insert logMessage (leftNode node)
  -- | (theTimestamp logMessage) > (theTimestamp (nodeMessage node)) = insert logMessage (rightNode node)
  -- | (theTimestamp logMessage) == (theTimestamp (nodeMessage node)) = node

-- insert (Unknown _) t = t
-- insert msg Leaf = Node Leaf msg Leaf
-- insert msg(LogMessage _ msgTime _) (Node ltree l@(LogMessage _ treeTime _) rtree)
--   | msgTime <= treeTime = Node (insert msg ltree) l rtree
--   | msgTime > treeTime = Node ltree l (insert msg rtree)
--
--   wrapping msg() around the first param lets us refer to msg later.
--    So when we create a node, you don't have to do something like my nodeMessage function
--   This function doesn't handle equality the same way I do, but the homework doesn't say how to do equality.
--   This function takes advantage of naming within the pattern match, which I forgot about.
--   Taking this example and mine, I'd probably now write
--
-- insert (Unknown _) tree = tree
-- insert logMessage (Leaf) = Node Leaf logMessage Leaf
-- insert msg(LogMessage _ msgTimeStamp _) node@(Node leftNode treeMessage@(LogMessage _ treeTimeStamp _) rightNode)
  -- | msgTimeStamp < treeTimeStamp = insert msg leftNode
  -- | msgTimeStamp logMessage > treeTimeStamp = insert msg rightNode
  -- | msgTimeStamp == treeTimeStamp = node
  --
-- My function also builds the tree wrong. 
-- insert (LogMessage _ 3 _ ) (Node (Node Leaf LogMessage _ 2 _ Leaf)  LogMessage _ 4 _ (Node Leaf LogMessage _ 5 _ Leaf))
--  = insert (LogMessage _ 3 _) (Node Leaf LogMessage _ 2 _ Leaf) 
--    = insert (LogMessage _ 3 _) (Leaf)
--      = Node Leaf (LogMessage _ 3 _) Leaf
--
-- And that's wrog. We lose the original tree entirely.
--
-- The example solution would owkr like this
-- insert (LogMessage _ 3 _ ) (Node (Node Leaf LogMessage _ 2 _ Leaf)  LogMessage _ 4 _ (Node Leaf LogMessage _ 5 _ Leaf))
--  = Node (insert (LogMessage _ 3 _) (Node Leaf LogMessage _ 2 _ Leaf)) LogMessage _ 4 _ (Node Leaf LogMessage _ 5 _ Leaf)
--    = Node (Node (insert (LogMessage _ 3 _) Leaf) LogMessage _ 2_ Leaf ) LogMessage _ 4 _ (Node Leaf LogMessage _ 5 _ Leaf)
--       = Node (Node (Node Leaf (LogMessage _ 3 _) Leaf) LogMessage _2_ Leaf) LogMessage _ 4 _ (Node Leaf LogMessage _5 _ Leaf)
--
-- which returns the full tree
--
--So, my function should be
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage (Leaf) = Node Leaf logMessage Leaf
insert msg(LogMessage _ msgTimeStamp _) node@(Node leftNode treeMessage@(LogMessage _ treeTimeStamp _) rightNode)
  | msgTimeStamp < treeTimeStamp = Node (insert msg leftNode) treeMessage rightNode
  | msgTimeStamp > treeTimeStamp = Node leftNode treeMessage (insert msg rightNode)
  | msgTimeStamp == treeTimeStamp = node

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [m] = Node Leaf m Leaf
build [m:ms] = insert m (build ms)

-- My first pass at Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf msg Leaf = [msg]
inOrder Leaf msg rnode = [msg] ++ (inOrder rnode)
inOrder lnode msg Leaf = (inOrder lnode) ++ [msg]
inOrder lnode msg rnode = (inOrder lnode) ++ [msg] ++ (inOrder rnode)

-- Given a tree like  (L 2 (L 3 L) 4 ((L 4.5 L) 5 L) this function resolves to
-- inOrder (L 2 (L 3 L) 4 ((L 4.5 L) 5 L)
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ inOrder ((L 4.5 L) 5 L)
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ (inOrder L 4.5 L) ++ [5]
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ [4.5] ++ [5]
--  = [2] ++ inOrder (L 3 L) ++ [4] ++ [4.5] ++ [5]
--  = [2] ++ [3] ++ [4] ++ [4.5] ++ [5]
--  = [2, 3, 4, 4.5, 5]

-- Reference solution for Exercise 4
-- inOrder :: MessageTree -> [LogMessage]
-- inOrder Leaf = []
-- inOrder (Node ltree msg rtree) = inOrder ltree ++ [msg] ++ inOrder rtree

-- Given a tree like  (L 2 (L 3 L) 4 ((L 4.5 L) 5 L) this function resolves to
-- inOrder (L 2 (L 3 L) 4 ((L 4.5 L) 5 L)
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ inOrder ((L 4.5 L) 5 L))
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ inOrder (inOrder (L 4.5 L) + [5] + inOrder (L)))
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ inOrder (inOrder (L 4.5 L) + [5] + []))
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ inOrder (inOrder (inOrder (L) ++ [4.5] ++ inOrder (L)) ++ [5] ++ []))
--  = inOrder (L 2 (L 3 L)) ++ [4] ++ [] ++ [4.5] ++ [] ++ [5] ++ []
--  ... and so on until
--  [] ++ [2] ++ [] ++ [3] ++ [] ++ [4] ++ [] ++ [4.5] ++ [] ++ [5] ++ []
--
--  Both functions end up working the same. The reference solution is shorter, but results in a lot of empty arrays being concatenated. Mine is longer, but has fewer cycles and empty arrays. The 'right' solution may be a matter of preference?
