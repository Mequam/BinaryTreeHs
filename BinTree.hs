module BinTree (someFunc) where
data BinaryTree a = Nada | Leaf a | Tree (BinaryTree a) (BinaryTree a) (BinaryTree a) deriving (Show)
--generic hook fuction
type Hook a = (a -> a)
type BoolCompare  a = (a -> a -> Bool)

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

--convinence wrapper for the most common use case of a binary tree 
binIns :: (Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a 
binIns = binInsGenLeaf (\x->x) (==) (less)

someFunc :: IO ()
someFunc = putStrLn "someFunc"