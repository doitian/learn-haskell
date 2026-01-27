data MessageType
  = Info
  | Warning
  | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage
  = LogMessage MessageType TimeStamp String
  | Unknown String
  deriving (Show, Eq)

data MessageTree
  = Leaf
  | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- |
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
--
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"

-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format
parseMessage :: String -> LogMessage
parseMessage = parseWords . words
  where
    parseWords ("E" : sev : ts : xs) = LogMessage (Error (read sev)) (read ts) (unwords xs)
    parseWords ("I" : ts : xs) = LogMessage Info (read ts) (unwords xs)
    parseWords ("W" : ts : xs) = LogMessage Warning (read ts) (unwords xs)
    parseWords xs = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

timeStamp :: LogMessage -> Maybe TimeStamp
timeStamp (LogMessage _ ts _) = Just ts
timeStamp _ = Nothing

-- |
-- >>> let root = Node Leaf (LogMessage Info 5 "info") Leaf
-- >>> insert (Unknown "u") root
-- Node Leaf (LogMessage Info 5 "info") Leaf
--
-- >>> let root = Node Leaf (LogMessage Info 5 "info") Leaf
-- >>> insert (LogMessage Warning 4 "info") root
-- Node (Node Leaf (LogMessage Warning 4 "info") Leaf) (LogMessage Info 5 "info") Leaf
--
-- >>> let root = Node Leaf (LogMessage Info 5 "info") Leaf
-- >>> insert (LogMessage (Error 1) 5 "info") root
-- Node Leaf (LogMessage Info 5 "info") (Node Leaf (LogMessage (Error 1) 5 "info") Leaf)
insert :: LogMessage -> MessageTree -> MessageTree
insert message tree = case timeStamp message of
  Just ts -> insertAtTimestamp message ts tree
  Nothing -> tree

insertAtTimestamp :: LogMessage -> TimeStamp -> MessageTree -> MessageTree
insertAtTimestamp message ts (Node left curr@(LogMessage _ currTs _) right)
  | ts < currTs = Node (insertAtTimestamp message ts left) curr right
  | otherwise = Node left curr (insertAtTimestamp message ts right)
insertAtTimestamp message ts _ = Node Leaf message Leaf

-- |
-- >>> let root = Node Leaf (LogMessage Info 5 "info") Leaf
-- >>> let tree = insert (LogMessage Warning 4 "info") root
-- >>> inOrder tree
-- [LogMessage Warning 4 "info",LogMessage Info 5 "info"]
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left curr right) = (inOrder left) ++ [curr] ++ (inOrder right)
inOrder Leaf = []
