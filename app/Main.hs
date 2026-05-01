module Main where

import Control.Monad.State.Strict (evalState)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Core.Compiler
import Core.LambdaLifting
import Core.Machine
import Core.Parser

runProg :: Text -> Text
runProg = showResults . evalState eval . compile . lambdaLift . parseCore

main :: IO ()
main = do
  fileName <- (!! 0) <$> getArgs
  program <- TIO.readFile fileName
  TIO.putStrLn . runProg $ program
