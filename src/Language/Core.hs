module Language.Core where

import Control.Monad.State.Strict (evalState)
import Data.Text (Text)

import Language.Core.Compiler
import Language.Core.Parser
import Language.Core.Prettyprint
import Language.Core.Machine

runProg :: Text -> Text
runProg = showResults . evalState eval . compile . parseCore
