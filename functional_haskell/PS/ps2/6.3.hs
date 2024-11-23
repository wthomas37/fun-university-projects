import Data.Foldable

data BinaryTree a 
    = EmptyTree 
    | Node a (BinaryTree a) (BinaryTree a) 
    deriving (Show)

tree :: BinaryTree Integer 
tree = Node 3
        (Node 1
            (Node 0 EmptyTree EmptyTree)
            (Node 2 EmptyTree EmptyTree))
        (Node 5
            (Node 4 EmptyTree EmptyTree)
            (Node 6 EmptyTree EmptyTree))

instance Foldable BinaryTree where
 -- foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldr combiner base EmptyTree =
      base
    foldr combiner base (Node a left right) =
      foldr combiner (combiner a (foldr combiner base right)) left


visit :: BinaryTree a -> [a] 
visit = foldr (:) []

reverseTree :: BinaryTree a -> BinaryTree a 
reverseTree EmptyTree = EmptyTree
reverseTree (Node a left right) = (Node a (reverseTree right) (reverseTree left))