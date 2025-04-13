{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State.Strict (evalState)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.HUnit (assertEqual, Counts(failures, errors), Test(..), runTestTT)
import qualified System.Exit as Exit

import Core.Compiler
import Core.Machine
import Core.Parser

testPath :: String
testPath = "test/examplePrograms/"

testPrograms :: [(String, Text)]
testPrograms = addPrefix testPath <$>
              [("arithmetic1.core", "7")
              , ("arithmetic2.core", "1024")
              , ("casejump.core", "2")
              , ("factorial.core", "3628800")
              , ("fibonazzi.core", "377")
              , ("gcd.core", "12")
              , ("letrec.core", "1")
              , ("undefined.core", "123")
              , ("logical1.core", "Pack{2,0}")
              , ("logical2.core", "Pack{2,0}")]
  where
    addPrefix prefix (path, result) = (prefix <> path, result)

extractResult :: [GmState] -> Text
extractResult states =  gmOutput $ last states

doTest :: Text -> Text -> Test
doTest prog expected = TestCase $
  assertEqual (T.unpack $ "should return " <> expected) expected (runProgram prog)

runProgram :: Text -> Text
runProgram = extractResult . evalState eval . compile . parseCore

main :: IO ()
main = do
  tl <- mapM (\(file, expected) -> do
                 prog <- TIO.readFile file
                 return $ TestLabel file (doTest prog expected)) testPrograms
  result <- runTestTT (TestList tl)
  if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
