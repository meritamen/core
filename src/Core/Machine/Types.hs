{-# LANGUAGE StrictData #-}

module Core.Machine.Types where

import Control.Monad.State.Strict
import Core.Utils
import Data.Map.Strict (Map)
import Data.Text (Text)

type GmEval = State GmState

data GmState = GmState
  { gmOutput :: GmOutput,
    gmCode :: GmCode,
    gmStack :: GmStack,
    gmDump :: GmDump,
    gmHeap :: GmHeap,
    gmGlobals :: GmGlobals,
    gmStats :: GmStats
  }
  deriving (Show)

type GmOutput = Text

type GmCode = [Instruction]

type GmStack = [Addr]

type GmDump = [GmDumpItem]

type GmDumpItem = (GmCode, GmStack)

type GmHeap = Map Addr Node

type GmGlobals = Map Name Addr

type GmStats = Int

data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int
  | Update Int
  | Pop Int
  | Alloc Int
  | Eval
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Cond GmCode GmCode
  | Pack Int Int
  | Casejump [(Int, GmCode)]
  | Split Int
  | Print
  | Not
  | And
  | Or
  deriving (Eq, Show)

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving Show
