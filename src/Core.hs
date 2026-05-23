module Core
  ( dumpCoreTrace,
    getCoreOutput,
    traceCoreProgram,
    extractResult,
  )
where

import Control.Monad.State.Strict
import Core.Compiler
import Core.LambdaLifting
import Core.Machine
import Core.Parser
import Data.Text (Text)

dumpCoreTrace :: Text -> Text
dumpCoreTrace = showResults . traceCoreProgram

getCoreOutput :: Text -> Text
getCoreOutput = extractResult . traceCoreProgram

traceCoreProgram :: Text -> [GmState]
traceCoreProgram = evalState eval . compile . lambdaLift . parseCore

extractResult :: [GmState] -> Text
extractResult = gmOutput . last
