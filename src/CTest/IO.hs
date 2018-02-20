{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
 module : IO
 description : Inupt/Output, preprocessing and also evaluation
 maintainer : Marius Gerdes, Matrikel-Nr.: 772451

This module makes up the interface between filesystem and user and the rest of the ctest program. Additionally, it also contains the functions for evaluating the programs performance.
-}
module CTest.IO 
  (getTestSolver,
  getEvaluator,
  getCorpus,
  preProcessTest,
  postProcessTest)
  where

-- Above (in parens) is a list of exported identifiers. All other identifiers declared in a module stay hidden when the module is imported.

import CTest.Bigram
import CTest.CTest
import CTest.WordMap

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath

-- * Exported Functions

getCorpus :: FilePath -> IO [Text]
-- ^Reads in a file and returns a list of lines in that file.
getCorpus filename = TIO.readFile filename >>= return . T.lines 

preProcessTest :: Text -> [Text]
-- ^Replaces newlines in a string with "." and returns a list of words in the input string.
-- This function is meant to preprocess ctests.
preProcessTest = concat . zipWith (flip $ (++) . T.words) (cycle [[T.pack "."]]) . T.lines

-- (!) T.pack converts String to Text.
-- cycle repeats a list infinitely
-- flip changes the order of arguments of a binary function

-- (!) A "where" is like a "let" binding, except that it is a declaration and not an expression
-- It is used below to declare the function f locally.
-- Guards ("|" or pipes) can be used below function declarations. They act as select case statements.

postProcessTest :: [Text] -> Text
-- ^Brings a preprocessed string back into its presentable form. Replaces "." with newlines and concatenates.
-- It should hold that postProcessTest . preProcessTest = id
postProcessTest = T.unwords . map f
  where f w
          | w == (T.pack ".") = T.pack "\n"
          | otherwise = w

getTestSolver :: FilePath -> IO (Text -> [Text])
-- ^Takes a path to a corpus text file and returns (in the IO monad) a function that
-- returns solutions to ctests
-- Solutions are returned as a list of words.
-- The corpus is used to build a Bigram probability distribution, as well as a Map from characters to Sets of strings.
-- From these, a function for solving ctests is built and returned. 
getTestSolver corpusfile =
  do
    ws <- getCorpus corpusfile
    let p = makeProbabilityDistributionFromCorpus ws  -- from CTest.Bigram
        wm = makeWordMap ws -- from CTest.WordMap
        f = makeTestsSolver p wm  -- from CTest.CTest
    return $ f . preProcessTest

-- ** Evaluation

-- Following are functions for evaluating the solving of ctests



getEvaluator :: FilePath -> IO (Text -> [Text] -> Text)
-- ^Takes a path to a gold standard (text file) and returns a function (in the IO monad)
-- that can be used to evaluate the success of a ctest-solving function (e.g. from makeTestsSolver).
-- The gold should look exactly like the ctest given on standard input, only with underlined words filled in.
getEvaluator goldpath =
  do
    gold <- TIO.readFile goldpath
    return $ evaluate (preProcessTest gold) . preProcessTest 

-- * Internal Functions


-- ** Data Types

-- | Represents the different success or failure states of each word in a ctest.
data Evaluation 
  -- |An underlined word has been filled in according to the gold standard. Success!
  = Success 
  -- | Filled-in word differs from the gold standard. The constructor takes a pair of the word from the standard and the mistakenly filled-in word as argument.
  | Failure (Text, Text) 
  -- |Most words in a ctest are not underlined (not to be solved). They are neither success nor failure.
  | Neither
  deriving (Show, Eq, Ord)

-- (!) The "deriving" keyword in a data declaration automatically makes types instances of specified type classes.
-- It is used merely for convenience here.


-- ** Evaluation

evaluateSingle :: (Text, Text, Text) -> Evaluation
-- ^Takes a 3-tuple of words:
--  The first from the gold standard
--  TThe second from the test supplied from standard input
--  and the third from a test solving function
-- These three are compared andd the appropriate Evaluation is returned.
evaluateSingle (gold, test, solution)
        | not $ T.isSuffixOf (T.pack "__") test = Neither
        | gold == solution = Success
        | otherwise = Failure (gold, solution)

evaluate :: [Text] -> [Text] -> [Text] -> Text
-- ^Like evaluateSingle but operates not on single 3-tuples but rather 
-- on 3 lists of words: Gold standard, user supplied ctest, and computed solution.
-- The three lists must have the same length for the evaluation to be meaningful.
-- Returns a fully formated text.
evaluate gold test solution = summarizeEvaluation $ foldr f init $ map evaluateSingle $ zip3 gold test solution
  where init@(attempts, successes, failures, failureList) = (0, 0, 0, []) 
        f Success (attempts, successes, x, y) = (attempts + 1, successes + 1, x, y)
        f (Failure mistake) (attempts, x, failures, failureList) = (attempts + 1, x, failures + 1, mistake:failureList)
        f Neither acc = acc

summarizeEvaluation :: (Int, Int, Int, [(Text, Text)]) -> Text
-- ^Takes a 4-tuple of numerical success-statistics and a list of failed solutions and returns a summarry as Text.
-- The list of failures contains pairs of words from the gold standard and computed, mistaken solutions that correspond to them.
summarizeEvaluation (attempts, successes, failures, failureList) = T.unlines [totals, rate, examples]
  where totals = T.unwords $ [conv attempts, f " solutions attempted\n", conv successes, f " successful\n", conv failures, f " failed\n"]
        rate = T.unwords $ [f "Success rate: ", conv $ (fromIntegral successes)/(fromIntegral attempts)]
        examples = T.unlines $ map (\ (gold, fail) -> T.unwords [gold, fail]) failureList
        conv x = T.pack $ show x  -- conversion from int to Text
        f = T.pack

