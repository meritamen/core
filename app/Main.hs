module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

import Language.Core

main :: IO ()
main = do
  fileName <- (!! 0) <$> getArgs
  program <- TIO.readFile fileName
  TIO.putStrLn . runProg $ program
