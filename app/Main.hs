module Main where

import Core
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  fileName <- (!! 0) <$> getArgs
  program <- TIO.readFile fileName
  TIO.putStrLn . dumpCoreTrace $ program
