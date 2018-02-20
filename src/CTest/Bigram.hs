{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
 module : CTest.Bigram
 description : Generation and application of statistical bigram models.
 maintainer : Marius Gerdes, Matrikel-Nr.: 772451

This module contains functions and data types for performing maximum likelihood training and applying of a statistical bigram model.
-}
module CTest.Bigram 
  (nGrams,
  biGrams,
  makeProbabilityDistributionFromCorpus,
  BigramProbabilityDistribution) where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

-- *Data Types

-- (!) as opposed to "data", the "type" keyword merely declares type synonyms

type Bigram = (Text, Text)
type Unigram = Text

-- |The type of the function used to find the specific probability of a given bigram.
type BigramProbabilityDistribution = Bigram -> Unigram -> Maybe Double

-- * Exported Functions

-- (!) The Maybe monad is used to handle computations that might fail.
-- Its type is Maybe a = Just a | Nothing
-- It is used as the return type of the probability function, because the lookup of a bigram might fail. 
-- In that case, the function returns Nothing.

nGrams :: Int -> [a] -> [[a]]
-- ^Returns a list of n-grams of a given list.
-- Example: nGrams 3 [1,2,3,4] 
-- = [[1,2,3],[2,3,4]]
nGrams n xs = (take $ (length xs) - (n - 1)) . map (take n) . tails $ xs

-- (!) tails returns all possible tails of a list, e.g.
-- tails [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4]]

biGrams :: [a] -> [(a, a)]
-- ^More specific version of nGrams for bigrams. Returns pairs instead of lists.
biGrams xs = (take ((length xs) - 1)) . map (\(a:b:_) -> (a, b)) $ tails xs

makeProbabilityDistributionFromCorpus :: [Text] -> BigramProbabilityDistribution
-- ^Returns a probability function for a given corpus.
makeProbabilityDistributionFromCorpus ws = probability bigramMap unigramMap
  where (bigramMap, unigramMap) = (count (biGrams ws) M.empty, count ws M.empty)

-- (!) M.empty is the empty Map, used here as initial value for count

-- * Internal Functions

-- (!) (Ord k) => means that in the following type declaration, the type variable k is a member of the Ord type class.
-- That means comparison operators are defined for k
-- This is necessary because Map is implemented using balanced binary trees.

-- (!) the type Map k Int defines a Map that uses k as keys and has Integer values

count :: (Ord k) => [k] -> Map k Int -> Map k Int
-- ^Given a list of ks and a (possibly empty) Map from k to Int,
-- returns a Map that has for every k the number of occurences in the original k-list stored as values.
count xs m = foldl' (flip (M.alter f)) m  $! xs
  where f Nothing = Just 1  -- key missing, insert and set count to 1
        f (Just n) = Just $! n + 1  -- key exists, increase count by 1

-- (!) M.alter is a function in the Map module.
-- Its type is alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
-- that is, it takes a function that can deal with missing keys, a key and then applies the function to the corresponding value for the key in a given Map
-- feeding the result into a new map
-- This way, it can insert, change or delete values - depending on the Maybe type constructor used.



probability :: Map Bigram Int -> Map Unigram Int -> BigramProbabilityDistribution
-- ^Returns a probability function based on given Maps of Bigrams and Unigrams.
-- This is where the actual calculations happen.
probability bigrams unigrams (w1, w2) w_uni = f (M.findWithDefault 0 (w1, w2) bigrams) (M.findWithDefault 0 w_uni unigrams)
  where f _ 0 = Nothing  -- no divide by zero
        f p1 p2 = Just $ (fromIntegral p1) / (fromIntegral p2) 

-- (!) fromIntegral is essentially type casting for different numerical types. There are no sneaky conversions in haskell.