{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
 module : CTest.WordMap
 description : Maps from characters to Sets of Text
 maintainer : Marius Gerdes, Matrikel-Nr.: 772451

This module provides functions for the generation and use of Maps, that map characters to Sets of Text (i.e. strings).
These are used as a heuristic in the solving of ctests. Since the first character of any to-be-solved word in a ctest is guaranteed to be present, it can be used to quickly retrieve and narrow down
a list of potential candidates for a solution.

Why Map?

The haskell provided Maps are implemented using balanced binary trees (O(log(n)) lookup and insertion).
I have used these, here and throughout, not because they are the most efficient for the given task, but because they are included in the standard.
Potentially more efficient data structures (hashtables, tris etc) do exist, but require third party libraries, which i did not want to use for this project.
-}
module CTest.WordMap 
  (WordMap,
  makeWordMap) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S

-- * Data Types
-- |A Map from characters to sets of words (Text).
-- The Set corresponding to a given key (a character) contain words whose first letter is that key (that character)
type WordMap = Map Char (Set Text)

-- * Exported Functions

makeWordMap :: [Text] -> WordMap
-- ^Given a corpus (as list of words), returns a Map from characters to Sets of words that begin with the character key.
makeWordMap = (flip collectWords) M.empty

-- * Internal Functions

collectWords :: [Text] -> WordMap -> WordMap
-- ^Takes a corpus as first argument and fills in the WordMap in the second argument.
collectWords ws m = foldl' f m ws
  where f m w
            | T.null w = m  -- empty string, do nothing
            | otherwise = let c = T.head w in M.alter (g w) c m
        g w Nothing = Just $ S.singleton w  -- char not yet in Map, create key and insert new Set with one element
        g w (Just s) = Just $ S.insert w s  -- char already in map; insert string into corresponding set and update entry

