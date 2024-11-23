import Prelude hiding (Monad, (>>=))

-- Monad is a subclass of applicative, which is a subclass of functor

class Applicative t => Monad_ (t :: * -> *) where 
    -- pure : a -> t a
    -- fmap :: (a -> b) -> t a -> t b
    -- (<*>) :: t (a -> b) -> t a -> t b
    andThen :: t a -> (a -> t b) -> t b 

instance Monad_ Maybe where
    andThen :: Maybe a -> (a -> Maybe b) -> Maybe b 
    andThen (Just a) f = f a 
    andThen _ _ = Nothing 

instance Monad_ [] where 
    andThen :: [a] -> (a -> [b]) -> [b] 
    -- andThen as f = concat $ map (\a -> f a) as 
    -- andThen as  f = concatMap (\a -> f a) as 
    -- andThen as f = concatMap f as 
    -- andThen as f = flip concatMap as f 
    andThen = flip concatMap
