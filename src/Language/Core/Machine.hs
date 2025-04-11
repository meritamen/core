{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Machine where

import Control.Monad (join)
import Control.Monad.State.Strict (State, put, get, gets)
import Data.Char (digitToInt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import TextShow (showt)

import Language.Core.Utils

type GmEval = State GmState

data GmState = GmState
  { gmOutput :: GmOutput
  , gmCode :: GmCode
  , gmStack :: GmStack
  , gmDump :: GmDump
  , gmHeap :: GmHeap
  , gmGlobals :: GmGlobals
  , gmStats :: GmStats }
  deriving Show

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
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GmCode GmCode
  | Pack Int Int
  | Casejump [(Int, GmCode)]
  | Split Int
  | Print
  deriving (Eq, Show)

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving (Eq, Show)

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
  Alloc n -> alloc' n
  Eval -> eval'
  Add -> arithmetic2 (+)
  Sub -> arithmetic2 (-)
  Mul -> arithmetic2 (*)
  Div -> arithmetic2 div
  Neg -> arithmetic1 negate
  Eq -> comparison (==)
  Ne -> comparison (/=)
  Lt -> comparison (<)
  Le -> comparison (<=)
  Gt -> comparison (>)
  Ge -> comparison (>=)
  Cond i1 i2 -> cond i1 i2
  Pack t n -> pack t n
  Casejump alts -> casejump alts
  Split n -> split n
  Print -> print'

pushglobal :: Name -> GmEval ()
pushglobal f | Just (t, n) <- matchPack f = do
                 st@GmState{..} <- get
                 if f `elem` Map.keys gmGlobals
                   then put st{ gmStack = gmGlobals Map.! f : gmStack }
                   else let t' = text2Int t
                            n' = text2Int n
                            (gmHeap', addr) = alloc gmHeap (NGlobal n' [Pack t' n', Update 0, Unwind])
                        in put st{ gmStack = addr : gmStack
                                 , gmHeap = gmHeap'
                                 , gmGlobals = Map.insert f addr gmGlobals }

             | True = do
                 { st@GmState{..} <- get; put st{ gmStack = gmGlobals Map.! f : gmStack } }

matchPack :: Text -> Maybe (Text, Text)
matchPack txt =
    case T.stripPrefix "Pack{" txt >>= T.stripSuffix "}" of
        Just inner ->
            case T.splitOn "," inner of
                [x, y] -> Just (T.strip x, T.strip y)
                _      -> Nothing
        Nothing -> Nothing

text2Int :: Text -> Int
text2Int text | [c] <- T.unpack text = digitToInt c

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
  put st{ gmStack = gmStack !! n : gmStack }

getArg :: Node -> Addr
getArg (NAp _ a2) = a2

slide :: Int -> GmEval ()
slide n = do
  { st@GmState{..} <- get; put st{ gmStack = head gmStack : drop n (tail gmStack) } }

unwind :: GmEval ()
unwind = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NNum _ -> put st{ gmCode = fst (head gmDump)
                    , gmStack = head gmStack : snd (head gmDump)
                    , gmDump = tail gmDump }
    NAp a1 _ -> put st{ gmCode = [Unwind], gmStack = a1 : gmStack }
    NInd a -> put st{ gmStack = a : tail gmStack, gmCode = [Unwind] }
    NGlobal n c -> if length gmStack - 1 < n
                     then put st{ gmCode = fst $ head gmDump
                                , gmStack = last gmStack : snd (head gmDump)
                                , gmDump = tail gmDump }
                     else put st{ gmStack = rearrange n gmHeap gmStack, gmCode = c }
    NConstr _ _ -> put st{ gmCode = fst $ head gmDump
                         , gmStack = head gmStack : snd (head gmDump)
                         , gmDump = tail gmDump }

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as = take n as' <> drop n as
    where
      as' = (getArg . (heap Map.!)) <$> tail as

update :: Int -> GmEval ()
update n = do
 st@GmState{..} <- get
 put st{ gmStack = tail gmStack, gmHeap = Map.insert (gmStack !! (n+1)) (NInd $ head gmStack) gmHeap }

pop :: Int -> GmEval ()
pop n = do { st@GmState{..} <- get; put st{ gmStack = drop n gmStack } }

alloc' :: Int -> GmEval ()
alloc' n = do
  st@GmState{..} <- get
  let (gmHeap', addrs) = allocNodes n gmHeap
  put st{ gmStack = addrs <> gmStack, gmHeap = gmHeap' }

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a:as)
  where
    (heap1, as) = allocNodes (n-1) heap
    (heap2, a) = alloc heap1 (NInd 0)

eval' :: GmEval ()
eval' = do
  st@GmState{..} <- get
  put st { gmCode = [Unwind], gmStack = [head gmStack], gmDump = (gmCode, tail gmStack) : gmDump }

boxInteger :: Int -> GmEval ()
boxInteger n = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap (NNum n)
  put st{ gmStack = addr : gmStack, gmHeap = gmHeap' }

unboxInteger :: Addr -> GmEval Int
unboxInteger a = do { GmState{..} <- get; return $ ub $ gmHeap Map.! a }
  where
    ub (NNum i) = i
    ub _ = error "Unboxing a non-integer"

primitive1 :: (b -> GmEval ()) -> (Addr -> GmEval a) -> (a -> b) -> GmEval ()
primitive1 box unbox op = do
  st@GmState{..} <- get
  int1 <- unbox $ head gmStack
  put st{ gmStack = tail gmStack }
  box $ op int1

primitive2 :: (b -> GmEval ()) -> (Addr -> GmEval a) -> (a -> a -> b) -> GmEval ()
primitive2 box unbox op = do
  st@GmState{..} <- get
  let (a0:a1:as) = gmStack
  int1 <- unbox a0
  int2 <- unbox a1
  put st{ gmStack = as }
  box $ op int1 int2

arithmetic1 :: (Int -> Int) -> GmEval ()
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GmEval ()
arithmetic2 = primitive2 boxInteger unboxInteger

boxBoolean :: Bool -> GmEval ()
boxBoolean b = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap (NNum b')
      b' | b = 1 | otherwise = 0
  put st{ gmStack = addr : gmStack, gmHeap = gmHeap' }

comparison :: (Int -> Int -> Bool) -> GmEval ()
comparison = primitive2 boxBoolean unboxInteger

cond :: GmCode -> GmCode -> GmEval ()
cond i1 i2 = do
  st@GmState{..} <- get
  if gmHeap Map.! head gmStack == NNum 1
    then put st{ gmCode = i1 <> gmCode, gmStack = tail gmStack }
    else put st{ gmCode = i2 <> gmCode, gmStack = tail gmStack }

pack :: Int -> Int -> GmEval ()
pack t n = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap $ NConstr t $ take n gmStack
  put st{ gmStack = addr : drop n gmStack, gmHeap = gmHeap' }

casejump :: [(Int, GmCode)] -> GmEval ()
casejump alts = do
  st@GmState{..} <- get
  put st{ gmCode = Map.fromList alts Map.! getConstrTag (gmHeap Map.! head gmStack) <> gmCode }

split :: Int -> GmEval ()
split n = do
  st@GmState{..} <- get
  put st{ gmStack = take n (getConstrAddrs (gmHeap Map.! head gmStack)) <> tail gmStack }

print' :: GmEval ()
print' = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NNum n -> put st{ gmOutput = gmOutput <> showt n }
    NConstr t as -> put st{ gmOutput = "Pack{" <> showt t <> "," <> showt (length as) <> "}"
                          , gmStack = as <> tail gmStack
                          , gmCode = join (replicate (length as) [Eval, Print]) <> gmCode }

getConstrTag :: Node -> Int
getConstrTag (NConstr t _) = t

getConstrAddrs :: Node -> [Addr]
getConstrAddrs (NConstr _ as) = as
