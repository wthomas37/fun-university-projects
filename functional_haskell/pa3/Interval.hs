{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use and" #-}
module Interval where
import Data.List hiding (union)

---- Intervals ----

data Interval a =
  Range a a

isEmptyInterval :: Ord a => Interval a -> Bool
isEmptyInterval (Range start end) =
  start >= end

instance Ord a => Eq (Interval a) where
  i1@(Range start1 end1) == i2@(Range start2 end2) =
    (isEmptyInterval i1 && isEmptyInterval i2) ||
    (start1 == start2 && end1 == end2)

-- Intervals are ordered by start. This is necessary for normalizeIS.
--
instance Ord a => Ord (Interval a) where
  Range start1 end1 <= Range start2 end2 =
    (start1, end1) <= (start2, end2)

instance (Show a, Ord a, Bounded a) => Show (Interval a) where
  show i@(Range start end)
    | isEmptyInterval i = "Empty"
    | start == minBound && end == maxBound = "All"
    | start == minBound = "<" ++ show end
    | end == maxBound = ">=" ++ show start
    | otherwise = show start ++ "<=_<" ++ show end

instance (Read a, Ord a, Bounded a) => Read (Interval a) where
  -- Read is a little less straightforward because it uses parsers,
  -- which we'll learn about later. Just replace the `undefined`s below
  -- with what you want to return and everything will work.
  -- Hint: Use `read` to get the value of an endpoint from `next`.
  readsPrec _ "Empty"        = [(Range minBound minBound, "")]
  readsPrec _ "All"          = [(Range minBound maxBound, "")]
  readsPrec _ ('>':'=':next) = [(Range (read next) maxBound, "")]
  readsPrec _ ('<':next)     = [(Range minBound (read next), "")]
  readsPrec _ str =
    -- Don't worry about this case. It is a bit clunky.
    -- We'll learn a better approach later in the course.
    case reads str of
      (start, '<':'=':'_':'<':rest):_ ->
        case reads rest of
          []        -> error "error parsing interval"
          (end,_):_ -> [(Range start end, "")]
      _ ->
        error "error parsing interval"


intersectIntervals :: Ord a => Interval a -> Interval a -> Interval a
intersectIntervals int1 int2 = 
  let sorted = sort [int1, int2] in 
    case sorted of 
      (Range _ end1) : [Range start2 end2] -> 
        if end2 < end1 then int2 
        else Range start2 end1 
      _ -> error "error finding intersections"


---- Interval Sets ----

-- An interval set might have intervals that overlap or touch. Don't worry
-- about simplifying these cases in the following functions. That is handled
-- just before displaying by normalizeIS.

type IntervalSet a =
  [Interval a]

toIS :: Interval a -> IntervalSet a
toIS =
  (:[])

emptyIS :: IntervalSet a
emptyIS =
  []

allIS :: Bounded a => IntervalSet a
allIS =
  [Range minBound maxBound]

removeEmptyIntervals :: Ord a => IntervalSet a -> IntervalSet a
removeEmptyIntervals =
  filter $ not . isEmptyInterval

intersectISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
intersectISI int_set int = removeEmptyIntervals . map (intersectIntervals int) $ int_set

-- The complement of an interval must return an interval set because it may
-- result in two disjoint intervals.
--
complementInterval :: (Bounded a, Ord a) => Interval a -> IntervalSet a
complementInterval int@(Range start end)
  | isEmptyInterval int = [Range minBound maxBound]
  | start == minBound && end == maxBound = [Range end start]
  | otherwise = [Range minBound start, Range end maxBound]


-- An interval minus an interval must return an interval set because the second
-- could cut a hole in the middle of the first.
--
-- Big Hint: Use complements and intersections.
--
-- IMPORTANT NOTE: There cannot be any empty intervals left over in the ouptut
-- of this function. Leaving them does not affect the results, but it may make
-- your program too slow! You are welcome to use removeEmptyIntervals for this.
--

differenceIntervals
  :: (Ord a, Bounded a)
  => Interval a
  -> Interval a
  -> IntervalSet a
differenceIntervals int1@(Range start end) int2 = 
  let complement = complementInterval $ intersectIntervals int1 int2 in 
    case complement of 
      [comp] -> removeEmptyIntervals [int1 | not $ isEmptyInterval comp]
      [Range _ c_start, Range c_end _] ->
        removeEmptyIntervals [Range start c_start, Range c_end end]
      _ -> error "error computing the difference"




-- Interval set minus an interval.
--
differenceISI
  :: (Ord a, Bounded a)
  => IntervalSet a
  -> Interval a
  -> IntervalSet a
differenceISI int_set int = 
  concatMap (`differenceIntervals` int) int_set 


---- Helpers for interval sets ----

-- Recursive helper; I should probably find a more elegant solution but 
-- this is the most intuitive option as of now

intersectionHelper
  :: Ord a
  => IntervalSet a
  -> IntervalSet a 
  -> IntervalSet a 
  -> IntervalSet a 
intersectionHelper (first:rest) set2 rv =
  intersectionHelper rest set2 ((++) rv $ intersectISI set2 first)
intersectionHelper [] _ rv = rv 


intersection
  :: Ord a
  => IntervalSet a
  -> IntervalSet a
  -> IntervalSet a
intersection set1 set2 = intersectionHelper set1 set2 [] 


union :: IntervalSet a -> IntervalSet a -> IntervalSet a
union set1 set2 = set1 ++ set2 


difference
  :: (Ord a, Bounded a)
  => IntervalSet a
  -> IntervalSet a
  -> IntervalSet a
difference set1 set2 = foldl differenceISI set1 set2


---- Queries on interval sets ----

intersectionAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
intersectionAll (first : rest) = foldr intersection first rest 
intersectionAll [] = []


unionAll :: [IntervalSet a] -> IntervalSet a
unionAll (first : rest) = foldr (++) first rest 

-- Subtract from the first interval set all the remaining interval sets.
--
differenceAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
differenceAll (first : rest) = foldl difference first rest 


---- Boolean Helpers ----

isEmpty :: Ord a => IntervalSet a -> Bool
isEmpty =
  null . removeEmptyIntervals

-- Hint: areDisjoint and isSubset are simpler than areEqual.
-- Use what you have already defined.

-- Two interval sets are disjoint if they do not overlap
--
areDisjoint :: (Ord a) => IntervalSet a -> IntervalSet a -> Bool
areDisjoint set1 set2 = isEmpty $ intersection set1 set2

-- Of the form 'is set1 a subset of set2'
isSubset :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
isSubset set1 set2 = 
  set2 `union` difference set1 set2 == set2 

areEqual :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
areEqual is1 is2 =
  is1 `isSubset` is2 && is2 `isSubset` is1


---- Boolean Queries ----

areAllDisjoint :: Ord a => [IntervalSet a] -> Bool
areAllDisjoint (first : rest)
  | foldr (&&) True (map (areDisjoint first) rest) = areAllDisjoint rest 
  | otherwise = False 
areAllDisjoint [] = True 



areAllEqual :: (Ord a, Bounded a) => [IntervalSet a] -> Bool
areAllEqual (first : rest) = foldr (&&) True (map (areEqual first) rest)
