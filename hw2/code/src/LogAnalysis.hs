{-# OPTIONS_GHC -Wall #-}

module LogAnalysis
    ( parseMessage
    , parse
    , insert
    , build
    , inOrder
    , whatWentWrong
    )
    where

import Data.Maybe (fromMaybe)
import Log

-- Exercise 1
parseInt :: String -> Maybe Int
parseInt s = case (reads s :: [(Int, String)]) of
               [(x, "")] -> Just x
               _ -> Nothing

parseMessageType :: String -> Maybe MessageType
parseMessageType s = case s of
                       "I" ->Just Info
                       "W" -> Just Warning
                       'E':' ':xs -> parseInt xs >>= \errorLevel -> Just $ Error errorLevel
                       _ -> Nothing

splitLog :: String -> Maybe (String, String, String)
splitLog msg = let takeN n = unwords . take n . words
                   dropN n = unwords . drop n . words
                in case msg of
                     'I':' ':_ ->Just  (takeN 1 msg, takeN 1 $ dropN 1 msg, dropN 2 msg)
                     'W':' ':_ -> Just (takeN 1 msg, takeN 1 $ dropN 1 msg, dropN 2 msg)
                     'E':' ':_ -> Just (takeN 2 msg, takeN 1 $ dropN 2 msg, dropN 3 msg)
                     _ -> Nothing


parseMessageHelper :: String -> Maybe LogMessage
parseMessageHelper msg = do
    (messageTypeString, timestampString, logString) <- splitLog msg
    messageType                                     <- parseMessageType messageTypeString
    timestamp                                       <- parseInt timestampString
    return $ LogMessage messageType timestamp logString

parseMessage :: String -> LogMessage
parseMessage msg = fromMaybe (Unknown msg) (parseMessageHelper msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- TODO: Use Parsec Instead :)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert LogMessage{} (Node _ (Unknown _ ) _) = error "Should not happen"
insert logMessage Leaf =  Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ t1 _) (Node l msg@(LogMessage _ t2 _) r)
  | t1 <= t2 = Node (insert logMessage l) msg r
  | otherwise = Node l msg (insert logMessage r)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l logMessage r) = inOrder l ++ [logMessage] ++ inOrder r

-- Exercise 5

sortLogMessage :: [LogMessage] -> [LogMessage]
sortLogMessage = inOrder . build

filterLogMessageFunction :: Int -> LogMessage -> Bool
filterLogMessageFunction severityThreshold (LogMessage (Error severity) _ _) = severityThreshold <= severity
filterLogMessageFunction _ _ = False

showLog :: LogMessage -> String
showLog (LogMessage _ _ x) = x
showLog (Unknown x) = x

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map showLog . sortLogMessage . filter (filterLogMessageFunction 50)
