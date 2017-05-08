module BinaryTree where
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

insert :: (Ord a)  => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node a left right)
    | x < a = Node a (insert x left) right
    | x >= a = Node a left (insert x right)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isBinary :: Tree a -> Bool
isBinary Empty = True
isBinary (Node a left right) = isBinary left && isBinary right

search :: (Eq a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node a left right)
    | x == a = True
    | otherwise = search x left || search x right

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node a left right) = isBalanced left && isBalanced right && (abs (height left - height right) < 2)

height:: Tree a -> Int
height Empty = 0
height (Node a left right)
    | heightL > heightR = heightL + 1
    | otherwise = heightR + 1
    where heightL = height left
          heightR = height right

data TraverseType = VLR | LVR | LRV | VRL | RVL | RLV deriving Eq

traverseTree :: TraverseType -> Tree a -> [a]
traverseTree _ Empty = []
traverseTree trType (Node a left right) =
    case trType of
        VLR -> [a] ++ trL ++ trR
        LVR -> trL ++ [a] ++ trR
        LRV -> trL ++ trR ++ [a]
        VRL -> [a] ++ trR ++ trL
        RVL -> trR ++ [a] ++ trL
        RLV -> trR ++ trL ++ [a]
        where trL = traverseTree trType left
              trR = traverseTree trType right

toString :: (Show a, Eq a) => Tree a -> String
toString Empty = ""
toString node@(Node a left right)
    | isLeaf node = showA
    | otherwise = showA ++ "(" ++ toString left ++ "," ++ toString right ++ ")"
    where showA = show a

leaves :: (Eq a) => Tree a -> [a]
leaves Empty = []
leaves node@(Node a left right)
    | isLeaf node = [a]
    | otherwise = leaves left ++ leaves right

isLeaf :: (Eq a) => Tree a -> Bool
isLeaf Empty = False
isLeaf (Node _ left right) =
    left == Empty
    && right == Empty

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Node a left right) = nnodes left + nnodes right + 1

nsum :: (Num a) => Tree a -> a
nsum Empty = 0
nsum (Node a left right) = a + nsum left + nsum right

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Empty = Empty
tmap f (Node a left right) = Node (f a) (tmap f left) (tmap f right)

remove :: (Ord a) => a -> Tree a -> Tree a
remove _ Empty = Empty
remove x (Node a left right) | x < a  = Node a (remove x left) right
remove x (Node a left right) | x > a  = Node a left (remove x right)
remove _ (Node _ Empty right) = right
remove _ (Node _ left Empty) = left
remove _ (Node _ left right) = Node min left (remove min right)
    where min = findMin right

findMin :: (Ord a) => Tree a -> a
findMin (Node a Empty right) = a
findMin (Node _ left _) = findMin left

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge Empty t = t
merge t Empty = t
merge fst snd = insertList fst (traverseTree LVR snd)

insertList :: (Ord a) => Tree a -> [a] -> Tree a
insertList tree [h] = insert h tree
insertList tree (h:t) = insertList (insert h tree) t

--test data
--      4
--   2     5
-- 1   2     7
--       3
testTree = insert 3 $ insert 7 $ insert 5 $ insert 1 $ insert 2 $ insert 2 $ insert 4 Empty
-- testTree' = insert 8 $ insert 2 $ insert 1 $ insert 1 $ insert 12 $ insert 7 $ insert 4 Empty


