module BinaryTreeHs.BinTree (BinaryTree(..),binInsGen,binInsGenLeaf,top,left,right,binIns,binSearch,binSearchTup,tree2arr,binContainsAllOf) where
data BinaryTree a = Nada | Leaf a | Tree (BinaryTree a) (BinaryTree a) (BinaryTree a) deriving (Show)
--generic hook fuction
type Hook a = (a -> a)
type BoolCompare a = (a -> a -> Bool)
type BoolCheck a = a -> Bool

--not the most efficient thing in the world because haskell has to create a NEW binary tree every time it runs
--but this works for our purposes

--return the binary tree with the inserted variable
--the functional arguments are as follows
--	Hook = what do we set the old value to when the values match
--	equ (second arg): testing for equality
--	lessThan (thrid arg): testing for less than
binInsGen :: Hook (BinaryTree a) -> BoolCompare a -> BoolCompare a -> a -> BinaryTree a -> BinaryTree a
binInsGen h equ lessThan val (Tree t1 (Leaf treeVal) t2)
	| val `equ` treeVal = Tree t1 (h (Leaf treeVal)) t2 
	| val `lessThan` treeVal = Tree (binInsGen h equ lessThan val t1) (Leaf treeVal) t2
	| otherwise = Tree t1 (Leaf treeVal) (binInsGen h equ lessThan val t2)
binInsGen h equ lessThan val (Leaf leafVal)
	| val `equ` leafVal = h (Leaf leafVal)
	| val `lessThan` leafVal = Tree (Leaf val) (Leaf leafVal) Nada
	| otherwise = Tree Nada (Leaf leafVal) (Leaf val)
binInsGen _ _ _ val Nada = (Leaf val)

--convinence wrapper for any time we need to pass a leaf instead of a value
binInsGenLeaf :: Hook (BinaryTree a) -> BoolCompare a -> BoolCompare a -> BinaryTree a -> BinaryTree a -> BinaryTree a
binInsGenLeaf a b c (Leaf val) d = binInsGen a b c val d

--need this for type constraints in an argument to a higher order function, I need to figure out how to use type constraints with just <
less :: (Ord a) => a -> a -> Bool
less x y = x < y

--simple function to get the top node of a binary tree
top :: BinaryTree a -> BinaryTree a
top (Tree _ val _ ) = val
top x = x

--simple function to get the left node of a binary tree
left :: BinaryTree a -> BinaryTree a
left (Tree l _ _) = l
left x = x

--simple function to get the right node of a binary tree
right :: BinaryTree a -> BinaryTree a
right (Tree _ _ r) = r
right x = x

--the less and equ checks determine if the value is less or equal to the value that we are searching for
--note we don't need to know that value, just how to check it
--search the tree in binary order using the given comparison function
binSearch :: BoolCheck a -> BoolCheck a -> BinaryTree a -> Maybe a
binSearch less equ (Tree l (Leaf x) r)
	| less x = binSearch less equ l
	| not (equ x) = binSearch less equ r
	| otherwise = Just x --we have found the value that we were searching for
binSearch _ _ (Leaf x) = Just x
binSearch _ _ Nada = Nothing

binContains :: (Ord a) => BinaryTree a -> a -> Bool
binContains tree x = (binSearch (>x) (x==) tree) /= Nothing
--snd that works with Maybe 
mbySnd :: Maybe (a,b) -> Maybe b
mbySnd Nothing = Nothing
mbySnd (Just x) = Just (snd x)

--this is a very not usefull funtion, but for edge cases where this could be needed
tree2arr :: BinaryTree a -> [a]
tree2arr (Tree l (Leaf v) r) = tree2arr l ++ (v : tree2arr r)
tree2arr (Leaf x) = [x]
tree2arr Nada = []

containsFalse :: [Bool] -> Bool
containsFalse (x:xl)
	| x == False = False
	| otherwise = containsFalse xl
containsFalse [] = True 
--not the most efficient algorithm in the world, but it is beutifly written down i an elegant manner
binContainsAllOf :: (Ord a) => BinaryTree a -> BinaryTree a -> Bool
binContainsAllOf t1 t2 = containsFalse [(binContains t1 x) | x <-(tree2arr t2)]

--usefull for taged data
--searches a binary tree of ordered tuples where the first element of the tupal can be ordered
binSearchTup :: (Ord a) => BinaryTree (a,b) -> a -> Maybe b
binSearchTup tree x = mbySnd (binSearch ((>x) . fst) ((==x) . fst) tree)

--convinence wrapper for the most common use case of a binary tree 
binIns :: (Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a 
binIns = binInsGenLeaf (\x->x) (==) (less)

--for debugging purposes
leafArr = [Leaf (x,show (x^2))|x<-[1..]]
genTree x = foldr binIns (Leaf (x `div` 2,(show ((x `div` 2)^2)))) (take x leafArr)
