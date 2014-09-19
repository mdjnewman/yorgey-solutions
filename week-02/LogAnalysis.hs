{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List.Split

getFirstInt :: String -> Int
getFirstInt = read . head . words

getStrAfterInt :: String -> String
getStrAfterInt r = drop (1 + (length . show . getFirstInt $ r)) r

getLevel :: String -> Maybe (MessageType, String)
getLevel s = case s of
    ('I':' ':r) -> Just (Info, r)
    ('W':' ':r) -> Just (Warning, r)
    ('E':' ':r) -> Just (Error (getFirstInt r), getStrAfterInt r)
    _       -> Nothing

parseRemainder :: MessageType -> String -> LogMessage
parseRemainder messageType line = LogMessage messageType (getFirstInt line) (getStrAfterInt line)

-- ##########

parseMessage :: String -> LogMessage
parseMessage line = case getLevel line of
    Nothing     -> Unknown line
    Just (l,r)  -> parseRemainder l r

-- ##########

parse :: String -> [LogMessage]
parse s = map parseMessage $ splitOn "\n" s

-- ##########

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf           = Node Leaf m Leaf
insert newMessage@(LogMessage _ newts _) (Node l oldMessage@(LogMessage _ ts _) r) 
    = if newts < ts then Node (insert newMessage l) oldMessage r else
     Node l oldMessage (insert newMessage r)

-- ##########

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- ##########

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- ##########

getMessage :: LogMessage -> String
getMessage (Unknown s) = s
getMessage (LogMessage _ _ s) = s

relevant :: LogMessage -> Bool
relevant (LogMessage (Error sev) _ _) = sev >= 50
relevant _ = False

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong ms = map getMessage (filter relevant (inOrder . build $ ms))
whatWentWrong = map getMessage . filter relevant . inOrder . build
