module BinaryTree (BinaryTree(Empty, Node), root, left, right, isEmpty) where

-- import BinaryTree (BinaryTree(Empty, Node))

data BinaryTree t = Empty | Node { root :: t, 
                                   left :: (BinaryTree t),
                                   right :: (BinaryTree t) }
                    deriving (Eq, Show)
                    
isEmpty :: BinaryTree t -> Bool
isEmpty Empty = True
isEmpty _ = False

maxSumPath :: (Num t, Ord t) => BinaryTree t -> t
maxSumPath Empty = 0
maxSumPath (Node x l r) = x + max (maxSumPath l) (maxSumPath r)

tree :: BinaryTree Int
tree = Node 1
        (Node 3 (Node 4 Empty Empty) Empty)
        (Node 6 (Node 5 Empty Empty) 
                (Node 8 Empty 
                            (Node 42 Empty Empty))
                            
bloom :: BinaryTree t -> BinaryTree t                            
bloom Empty = Empty
bloom (Node x Empty Empty) = Node x newLeaf newLeaf
  where newLeaf = Node x Empty Empty

bloom (Node x l r) = Node x (bloom l) (bloom r)

prune :: BinaryTree t -> BinaryTree t                            
prune Empty = Empty
prune (Node _ Empty Empty) = Empty

prune (Node x l r) = Node x (prune l) (prune r)


data BST t = BEmpty | BNode t (BST t) (BST t) deriving Show

bstInsert :: Ord t => t -> BST t -> BST t
bstInsert x BEmpty = BNode x BEmpty BEmpty
bstInsert x t@(BNode y l r)
  | x < y = BNode y (bstInsert x l) r
  | x > y = BNode y l (bstInsert x r)
  | otherwise = t
  
bstFromList :: Ord t => [t] -> BST t
bstFromList = foldl (flip bstInsert) BEmpty