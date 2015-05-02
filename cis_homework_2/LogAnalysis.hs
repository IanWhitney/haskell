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
errorCode (id, _, _) = id
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

