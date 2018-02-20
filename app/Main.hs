{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
 module : Main
 description : Main entry point.
 maintainer : Marius Gerdes, Matrikel-Nr.: 772451

Contains only the main entry point of the program.
-}
module Main where

import CTest.IO

-- (!) haskell's native String type is a simple list of characters. Text is a more efficient implementation of Strings (unicode aware)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO
import System.Exit

-- (!) The following is a type declaration.
-- They are not strictly necessary, as the compiler will infer the type of an expression.
-- This happens mostly with lambda and locally defined functions.

usage :: String
-- ^String to be printed on the command line.
usage = unlines ["Takes ctests from standard input and prints solution to standard output.", 
                   "Usage: ctest [-e GOLDSTANDARD] CORPUSFILE",
                   "\nExample:\nctest data/tiger < data/ctest.txt\n\nOptions\n  -e - Evaluate; do not print solutions to stdout.\n    Instead prints out performance statistics and a list of errorpairs (goldstandard and mistaken guess)"]

-- (!) As with other functional languages, functions are first class.
-- All functions are curried by default; partial application of functions is commonplace.
-- also: haskell is pure, there are no mutable values


-- (!) haskell is pure and allows no side effects in computations. Therefore, interactions with the outside world (reading, writing etc) happen inside of the IO monad - hence the return type of Main
-- "do" notation can be used with monads. It mimics an imperative style, but is really just syntactic sugar for chaining the monadic bind operator (>>=) together.

main :: IO ()
-- ^Main entry point
main =
  do
    args <- getArgs
    case args of
      ["-e", gold, corpus] ->  -- evaluate program performance instead of printing solutions
        do
          evaluate <- getEvaluator gold
          doTests <- getTestSolver corpus
          TIO.interact $ \test -> evaluate test $ doTests test
      [corpus] ->  -- print solutions to ctests
        do
          doTests <- getTestSolver corpus
          TIO.interact $ postProcessTest . doTests
      _ -> 
        do
          name <- getProgName
          hPutStr stderr $ (name ++ usage)
          exitFailure

-- (!) haskell has pattern matching. This is used here in the case statements, and elsewhere in function declarations etc.
-- It is comparable to prolog's pattern matching.
-- pattern matching interacts with data constructors.
-- (:) is the List type data constructor (cons)

-- (!) T.interact takes a function as argument. It applies this function to standard input and prints the result to standard output.

-- (!) The ($) operator is just function application. It (mostly) does nothing, except save parentheses.
-- e.g. (f (g h) and (f $ g h) are equivalent
-- Its corollary, the (.) operator is function composition.

-- (!) 