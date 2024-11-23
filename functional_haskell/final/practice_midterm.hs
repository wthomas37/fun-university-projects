undup :: Eq a => [a] -> [a] 
undup (first : (second : rest))
    | first == second = undup (first : rest) 
    | otherwise = first : undup (second : rest)
undup [b] = [b] 
undup [] = [] 

data Sorted = Ascending | Descending
    deriving (Show)

maybeSorted :: Ord a => [a] -> Maybe Sorted 
maybeSorted list 
    | length list /= length (undup list) = Nothing 
    | otherwise = 
        case list of 
            [] ->  Just Ascending 
            [_] -> Just Ascending 
            (first : (second : _)) -> 
                if first > second then helper Descending list 
                else helper Ascending list 
                where 
                    helper Ascending (first : (second : rest))
                        | first < second = helper Ascending (second : rest)
                        | otherwise = Nothing 
                    helper Descending (first : (second : rest))
                        | first > second = helper Descending (second : rest)
                        | otherwise = Nothing 
                    helper sort_val [_] = Just sort_val 

data Tree a = Node a [Tree a]
    deriving (Show)

instance Functor Tree where 
    fmap f (Node a nodes) = Node (f a) (map (fmap f) nodes)
