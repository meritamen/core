{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Core.Machine where

import Control.Monad.State.Strict (State, put, get, gets)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Core.Utils

type GmEval = State GmState

data GmState = GmState
  { gmCode :: GmCode
  , gmStack :: GmStack
  , gmHeap :: GmHeap
  , gmGlobals :: GmGlobals
  , gmStats :: GmStats }
  deriving Show

type GmCode = [Instruction]
type GmStack = [Addr]
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
  deriving (Eq, Show)

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  deriving Show

alloc :: GmHeap -> Node -> (GmHeap, Addr)
alloc heap node = if Map.null heap
  then (Map.fromList [(1, node)], 1)
  else let (maxAddr, _) = Map.findMax heap
       in (Map.insert (maxAddr+1) node heap, maxAddr+1)

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps = (+1)

statGetSteps :: GmStats -> GmStats
statGetSteps = id

eval :: GmEval [GmState]
eval = reverse <$> evalHelper []
  where
    evalHelper states = do
      { step; doAdmin; state <- get; isFinished <- gmFinal;
      if isFinished then return $ state : states
      else evalHelper $ state : states }

doAdmin :: GmEval ()
doAdmin = do { st@GmState{..} <- get; put st{ gmStats = statIncSteps $ gmStats } }

gmFinal :: GmEval Bool
gmFinal = null <$> gets gmCode

step :: GmEval ()
step = do
  { st@GmState{..} <- get; put st{ gmCode = tail gmCode }; dispatch $ head gmCode }

dispatch :: Instruction -> GmEval ()
dispatch = \case
  Pushglobal f -> pushglobal f
  Pushint f -> pushint f
  Mkap -> mkap
  Push n -> push n
  Slide n -> slide n
  Unwind -> unwind
  Update n -> update n
  Pop n -> pop n

pushglobal :: Name -> GmEval ()
pushglobal f = do
  { st@GmState{..} <- get; put st{ gmStack = gmGlobals Map.! f : gmStack } }

pushint :: Int -> GmEval ()
pushint n = do
  st@GmState{..} <- get
  let (gmHeap', a) = alloc gmHeap (NNum n)
  put st{ gmStack = a : gmStack, gmHeap = gmHeap' }

mkap :: GmEval ()
mkap = do
  st@GmState{..} <- get
  let (a1:a2:as) = gmStack
      (gmHeap', a) = alloc gmHeap (NAp a1 a2)
  put st{ gmStack = a : as, gmHeap = gmHeap' }

push :: Int -> GmEval ()
push n = do
  st@GmState{..} <- get
  put st{ gmStack = (getArg $ gmHeap Map.! (gmStack !! (n+1))) : gmStack }

getArg :: Node -> Addr
getArg (NAp _ a2) = a2

slide :: Int -> GmEval ()
slide n = do
  { st@GmState{..} <- get; put st{ gmStack = head gmStack : drop n (tail gmStack) } }

unwind :: GmEval ()
unwind = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NNum _ -> put st
    NAp a1 _ -> put st{ gmCode = [Unwind], gmStack = a1 : gmStack }
    NGlobal n c -> if length gmStack - 1 < n
                      then error "Unwinding with too few arguments"
                      else put st { gmCode = c }
    NInd a -> put st{ gmStack = a : tail gmStack, gmCode = [Unwind] }

update :: Int -> GmEval ()
update n = do
 st@GmState{..} <- get
 put st{ gmStack = tail gmStack, gmHeap = Map.insert (gmStack !! (n+1)) (NInd $ head gmStack) gmHeap }

pop :: Int -> GmEval ()
pop n = do { st@GmState{..} <- get; put st{ gmStack = drop n gmStack } }
