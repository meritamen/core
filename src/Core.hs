module Core where

import Control.Monad.State.Strict
import Core.Backend.Codegen
import Core.LambdaLifting
import Core.Machine
import Core.Machine.Types
import Core.Machine.Pretty
import Core.Frontend.Parser
import Data.Text (Text)

dumpCoreTrace :: Text -> Text
dumpCoreTrace = showResults . traceCoreProgram

getCoreOutput :: Text -> Text
getCoreOutput = extractResult . traceCoreProgram

traceCoreProgram :: Text -> [GmState]
traceCoreProgram = evalState eval . compile . lambdaLift . parseCore

extractResult :: [GmState] -> Text
extractResult = gmOutput . last
