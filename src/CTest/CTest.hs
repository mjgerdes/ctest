{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
 module : CTest
 description : Parsing and solving ctests.
 maintainer : Marius Gerdes, Matrikel-Nr.: 772451

This module uses probability functions from CTest.Bigrams and a mapping from characters to sets of words from CTest.WordMap to parse and solve ctests in the form of lists of words.
It exports only one function, which can be used to solve ctests or return a function that solves them.
-}
module CTest.CTest 
  (makeTestsSolver) where

import CTest.Bigram
import CTest.WordMap

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

-- * Data Types

-- |CTestString is the type that a given ctest-text will be converted to. It is either a Word, to be ignored, or a Hole, to be filled in.
data CTestString
  -- |Word takes a Text as argument. It represents any non-underscored word in a ctest.
  = Word Text 
  -- |Hole represents a part of the ctest that is to be solved. It takes
-- a Char and a Text as arguments, the first letter of an underscored word and the entire word (without underscores, i.e. the hint) respectively.
  | Hole (Char, Text)
  deriving (Show, Eq, Ord)

-- * Exported Functions

makeTestsSolver :: BigramProbabilityDistribution -> WordMap -> [Text] -> [Text]
-- ^Given a bigram probability function and a Map from characters to lists of Text, returns a function that can solve ctests.
-- The returned function takes a list of words (preprocessed) and gives a solved list of words back.
makeTestsSolver p m = solveTests (solve p m) . parseTest

-- * Internal Functions

parseTest :: [Text] -> [CTestString]
-- ^Takes a list of words and differentiates between normal, uninteresting words and underscored ones, to be solved.
parseTest = map f
  where f w
          | T.null w = Word T.empty
          | T.pack "__" `T.isSuffixOf` w = Hole (T.head w, fst $ T.breakOn (T.pack "_") w)
          | otherwise = Word w

maximum' = foldl1' max

solve :: BigramProbabilityDistribution -> WordMap -> (Text, (Char, Text), Text) -> Text
-- ^Returns the solution to a single ctest word.
-- solve first takes a probability function and a WordMap (fromChar to [Text]).
-- It then takes a tuple of a word preceding the ctest hole, a pair and the word following the hole (essentially a trigram.
-- The pair consists of a Char - the first letter of the underscored ctest-hole
-- and a hint string - the entirety of alphanumeric characters preceding the underscores. 
-- The solution is attained by picking the best candidate based on the
-- average of forward and backward probability of the (prefix, hole) and (hole, suffix) bigrams.
-- In addition, the list of candidates is retrieved from the WordMap, based on the first character of the hint.
-- The list of candidates is then filtered with the full hint string, before finding the best candidate.
solve p m (prefix, (c, hint), suffix) 
       | null words = T.pack "FAIL"
       | otherwise = snd $ maximum' $ map calcProb words
  where words = filter (T.isPrefixOf hint) $ maybe [] S.toList $ M.lookup c m
        calcProb w = (pForward prefix w + pBackward suffix w / 2, w)
        pForward w1 w2 = f $ p (w1,w2) w1
        pBackward w2 w1 = f $ p (w1,w2) w2
        f = fromMaybe 0

-- (!) Maps return values in the Maybe monad, in case of lookup failure; the maybe function is used to supply a default value (here the empty list)
-- same with fromMaybe, only it does not take a function as argument

solveTests :: ((Text, (Char, Text), Text) -> Text) -> [CTestString] -> [Text]
-- ^Takes a function that solves a single ctest and then applies it to a list of CTestStrings (as list of words, not lines).
-- CTestStrings are unpacked appropriately for the solving function.
solveTests solver = map f . fixStartAndEnd . nGrams 3  
  where f (Word w1:Hole c:Word w2:_) = solver (w1, c, w2)  -- the interesting case, a hole preceded and followed by a word
        f (_:Word w:_) = w  -- other cases are uninteresting, simply return the middle of the trigram
        f _ = T.empty

fixStartAndEnd :: [[CTestString]] -> [[CTestString]]
-- ^Adds the proper start and end to a list of Trigrams (returned by nGrams 3).
-- Inserts the magic symbol ".", to denote start and end of sentences.
-- Example: [[a,b,c],[b,c,d]]
-- becomes [[.,a,b],[a,b,c],[b,c,d],[c,d,.]]
fixStartAndEnd xs@([first, second, _]:_) = [magicsymbol, first, second] : fixEnd xs
  where fixEnd [] = []
        fixEnd xs = let [_, second, third] = last xs in xs ++ [[second, third, magicsymbol]]
        magicsymbol = Word $ T.pack "."
fixStartAndEnd x = x  -- if we do not get trigrams, do nothing 
