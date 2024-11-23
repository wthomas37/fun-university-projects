{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
-- {-# HLINT ignore "Use zipWith" #-}

applyFunctionsPairwise :: [a -> b] -> [a] ->[b]
-- applyFunctionsPairwise fs as = 
-- applyFunctionsPairwise (f:fs) (a:as) = f a : applyFunctionsPairwise fs as 
-- applyFunctionsPairwise _ _ = []
-- applyFunctionsPairwise fs as = map (\(f, a) -> f a) $ zip fs as 
-- applyFunctionsPairwise fs as = map (\fa -> uncurry ($) fa) $ zip fs as 
-- applyFunctionsPairwise fs as = map (uncurry ($)) $ zip fs as 
-- applyFunctionsPairwise fs as = zipWith ($) fs as 
applyFunctionsPairwise = zipWith ($) 

-- newtype ZipList a = ZipList [a]

-- instance Functor ZipList where
--     fmap :: (a -> b) -> ZipList a -> ZipList b 
--     fmap f (ZipList as) = ZipList $ fmap @[] f as

-- instance Applicative_ ZipList where
--     apply :: [a -> b] -> [a] -> [b]
--     apply (ZipList fs) (ZipList as) = 
--         ZipList (zipWith ($) fs as)

