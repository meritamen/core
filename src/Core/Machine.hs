{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Core.Machine
  ( GmState (..)
  , GmOutput
  , GmCode
  , GmStack
  , GmDump
  , GmDumpItem
  , GmHeap
  , GmGlobals
  , GmStats
  , Instruction (..)
  , Node (..)
  , alloc
  , statInitial
  , statIncSteps
  , statGetSteps
  , eval
  , showResults
  ) where

import           Control.Monad              (join)
import           Control.Monad.State.Strict (State, get, gets, put)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prettyprinter
    (Doc, indent, line, pretty, punctuate, vsep, (<+>))
import           Text.Read                  (readMaybe)
import           TextShow                   (showt)

import Core.Utils

type GmEval = State GmState

data GmState = GmState
  { gmOutput  :: GmOutput
  , gmCode    :: GmCode
  , gmStack   :: GmStack
  , gmDump    :: GmDump
  , gmHeap    :: GmHeap
  , gmGlobals :: GmGlobals
  , gmStats   :: GmStats }
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
doAdmin = do
  st@GmState{..} <- get
  put st{ gmStats = statIncSteps $ gmStats }

gmFinal :: GmEval Bool
gmFinal = null <$> gets gmCode

step :: GmEval ()
step = do
  st@GmState{..} <- get
  put st{ gmCode = tail gmCode }
  dispatch $ head gmCode

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
  Not -> logical1 not
  And -> logical2 (&&)
  Or -> logical2 (||)

pushglobal :: Name -> GmEval ()
pushglobal f | Just (t, n) <- extractPack f = do
                 st@GmState{..} <- get
                 if f `elem` Map.keys gmGlobals
                   then put st{ gmStack = gmGlobals Map.! f : gmStack }
                   else let (gmHeap', addr) = alloc gmHeap $ NGlobal n [Pack t n, Update 0, Unwind]
                        in put st{ gmStack = addr : gmStack
                                 , gmHeap = gmHeap'
                                 , gmGlobals = Map.insert f addr gmGlobals }
             | otherwise = do
                 st@GmState{..} <- get
                 put st{ gmStack = gmGlobals Map.! f : gmStack }

extractPack :: Text -> Maybe (Int, Int)
extractPack text
    | "Pack{" `T.isPrefixOf` text && "}" `T.isSuffixOf` text =
        let
            innerText = T.drop 5 (T.dropEnd 1 text)
            parts = T.splitOn "," innerText
        in case parts of
            [nStr, mStr] -> (,) <$> readMaybe (T.unpack nStr) <*> readMaybe (T.unpack mStr)
            _ -> Nothing
    | otherwise = Nothing

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
  st@GmState{..} <- get
  put st{ gmStack = head gmStack : drop n (tail gmStack) }

unwind :: GmEval ()
unwind = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NNum _ -> put st{ gmCode = fst $ head gmDump
                    , gmStack = head gmStack : snd (head gmDump)
                    , gmDump = tail gmDump }
    NConstr _  _ -> put st{ gmCode = fst $ head gmDump
                          , gmStack = head gmStack : snd (head gmDump)
                          , gmDump = tail gmDump }
    NAp a1 _ -> put st{ gmCode = [Unwind], gmStack = a1 : gmStack }
    NInd a -> put st{ gmStack = a : tail gmStack, gmCode = [Unwind] }
    NGlobal n c -> if length gmStack - 1 < n
                   then put st{ gmCode = fst $ head gmDump
                              , gmStack = head gmStack : snd (head gmDump)
                              , gmDump = tail gmDump }
                   else put st{ gmStack = rearrange n gmHeap gmStack, gmCode = c }

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as = take n as' <> drop n as
    where
      as' = (getArg . (heap Map.!)) <$> tail as

update :: Int -> GmEval ()
update n = do
 st@GmState{..} <- get
 put st{ gmStack = tail gmStack
       , gmHeap = Map.insert (gmStack !! (n+1)) (NInd $ head gmStack) gmHeap }

pop :: Int -> GmEval ()
pop n = do
  st@GmState{..} <- get
  put st{ gmStack = drop n gmStack }

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
  put st{ gmCode = [Unwind]
        , gmStack = [head gmStack]
        , gmDump = (gmCode, tail gmStack) : gmDump }

boxInteger :: Int -> GmEval ()
boxInteger n = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap (NNum n)
  put st{ gmStack = addr : gmStack, gmHeap = gmHeap' }

unboxInteger :: Addr -> GmEval Int
unboxInteger a = do
  GmState{..} <- get
  return . ub $ gmHeap Map.! a
  where
    ub (NNum i) = i

primitive1 :: (b -> GmEval ()) -> (Addr -> GmEval a) -> (a -> b) -> GmEval ()
primitive1 box unbox op = do
  st@GmState{..} <- get
  put st{ gmStack = tail gmStack }
  int1 <- unbox $ head gmStack
  box $ op int1

primitive2 :: (b -> GmEval ()) -> (Addr -> GmEval a) -> (a -> a -> b) -> GmEval ()
primitive2 box unbox op = do
  st@GmState{..} <- get
  let (a0:a1:as) = gmStack
  put st{ gmStack = as }
  int1 <- unbox a0
  int2 <- unbox a1
  box $ op int1 int2

arithmetic1 :: (Int -> Int) -> GmEval ()
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GmEval ()
arithmetic2 = primitive2 boxInteger unboxInteger

boxBoolean :: Bool -> GmEval ()
boxBoolean b = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap (NConstr b' [])
  put st{ gmStack = addr : gmStack, gmHeap = gmHeap' }
    where
      b' | b = 2 | otherwise = 1

comparison :: (Int -> Int -> Bool) -> GmEval ()
comparison = primitive2 boxBoolean unboxInteger

cond :: GmCode -> GmCode -> GmEval ()
cond i1 i2 = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NConstr 2 [] -> put st{ gmStack = tail gmStack, gmCode = i1 <> gmCode }
    NConstr 1 [] -> put st{ gmStack = tail gmStack, gmCode = i2 <> gmCode }

pack :: Int -> Int -> GmEval ()
pack t n = do
  st@GmState{..} <- get
  let (gmHeap', addr) = alloc gmHeap $ NConstr t $ take n gmStack
  put st{ gmStack = addr : drop n gmStack, gmHeap = gmHeap' }

casejump :: [(Int, GmCode)] -> GmEval ()
casejump alts = do
  st@GmState{..} <- get
  put st{ gmCode = Map.fromList alts Map.!  getNConstrTag (gmHeap Map.! head gmStack) <> gmCode }

getNConstrTag :: Node -> Int
getNConstrTag (NConstr t _) = t

split :: Int -> GmEval ()
split n = do
  st@GmState{..} <- get
  put st{ gmStack = take n (getNConstrAddrs (gmHeap Map.! head gmStack)) <> tail gmStack }

getNConstrAddrs :: Node -> [Addr]
getNConstrAddrs (NConstr _ addrs) = addrs

print' :: GmEval ()
print' = do
  st@GmState{..} <- get
  case gmHeap Map.! head gmStack of
    NNum n -> put st{ gmOutput = gmOutput <> showt n }
    NConstr t as -> put st{ gmOutput = "Pack{" <> showt t <> "," <> showt (length as) <> "}"
                          , gmStack = as <> tail gmStack
                          , gmCode = join (replicate (length as) [Eval, Print]) <> gmCode }

logical1 :: (Bool -> Bool) -> GmEval ()
logical1 op = do
  st@GmState{..} <- get
  let result = bool2Node $ op $ node2Bool $ gmHeap Map.! head gmStack
      (gmHeap', addr) = alloc gmHeap result
  put st{ gmStack = addr : tail gmStack, gmHeap = gmHeap' }

logical2 :: (Bool -> Bool -> Bool) -> GmEval ()
logical2 op = do
  st@GmState{..} <- get
  let (a0:a1:as) = gmStack
      result = bool2Node $ op (node2Bool (gmHeap Map.! a0)) (node2Bool (gmHeap Map.! a1))
      (gmHeap', addr) = alloc gmHeap result
  put st{ gmStack = addr : as, gmHeap = gmHeap' }

node2Bool :: Node -> Bool
node2Bool (NConstr 1 []) = False
node2Bool (NConstr 2 []) = True

bool2Node :: Bool -> Node
bool2Node True  = NConstr 2 []
bool2Node False = NConstr 1 []

showResults :: [GmState] -> Text
showResults states = T.pack . show $
  "Supercombinator definitions" <> line
  <> mconcat (punctuate line (pSC s <$> Map.toList (gmGlobals s)))
  <> line <> line <> "State transitions" <> line <> line
  <> vsep (pState <$> states)
  <> line <> line
  <> pStats (last states)
  where
    (s:_) = states

pSC :: GmState -> (Name, Addr) -> Doc ann
pSC s (name, addr) =
  "Code for" <+> pretty name <> line <> pInstructions code <> line
  where
    (NGlobal _ code) = gmHeap s Map.! addr

pInstructions :: GmCode -> Doc ann
pInstructions is =
  "  Code:{"
  <> indent 1 (mconcat (punctuate ", " (pInstruction <$> is)))
  <+> "}" <> line

pInstruction :: Instruction -> Doc ann
pInstruction = \case
  Unwind -> "Unwind"
  Pushglobal f -> "Pushglobal" <+> pretty f
  Push n -> "Push" <+> pretty n
  Pushint n -> "Pushint" <+> pretty n
  Mkap -> "Mkap"
  Slide n -> "Slide" <+> pretty n
  Update n -> "Update" <+> pretty n
  Pop n -> "Pop" <+> pretty n
  Alloc n -> "Alloc" <+> pretty n
  Eval -> "Eval"
  Add -> "Add"
  Sub -> "Sub"
  Mul -> "Mul"
  Div -> "Div"
  Neg -> "Neg"
  Eq -> "Eq"
  Ne -> "Ne"
  Lt -> "Lt"
  Le -> "Le"
  Gt -> "Gt"
  Ge -> "Ge"
  Cond i1 i2 -> "Cond" <+> "[" <> mconcat (punctuate ", " (pInstruction <$> i1)) <> "]"
                       <+> "[" <> mconcat (punctuate ", " (pInstruction <$> i2)) <> "]"
  Pack n0 n1 -> "Pack" <+> pretty n0 <+> pretty n1
  Casejump alts ->
    "Casejump" <+> "[" <>
    mconcat ((\(tag, code) ->
                pretty tag <+> "->"
                <+> "[" <> mconcat (punctuate ", " (pInstruction <$> code)) <> "]") <$> alts)
    <+> "]"
  Split n -> "Split" <+> pretty n
  Print -> "Print"
  Not -> "Not"
  And -> "And"
  Or -> "Or"

pState :: GmState -> Doc ann
pState s = pOutput s <> line
           <> pStack s <> line
           <> pDump s <> line
           <> pInstructions (gmCode s) <> line

pStack :: GmState -> Doc ann
pStack s =
  " Stack:["
  <> indent 1 (mconcat (punctuate ", " (pStackItem s <$> gmStack s)))
  <+> "]"

pStackItem :: GmState -> Addr -> Doc ann
pStackItem s a = pretty (showAddr a) <> ":" <+> pNode s a (gmHeap s Map.! a)

pNode :: GmState -> Addr -> Node -> Doc ann
pNode _ _ (NNum n) = pretty n
pNode s a (NGlobal _ _) = "Global" <+> pretty (head [n | (n, b) <- Map.toList $ gmGlobals s, a == b])
pNode _ _ (NAp a1 a2) = "Ap" <+> pretty (showAddr a1) <+> pretty (showAddr a2)
pNode _ _ (NInd a) = "Ind" <+> pretty (showAddr a)
pNode _ _ (NConstr t as)
  = "Cons" <+> pretty t <+> "[" <> mconcat (punctuate ", " (pretty . showAddr <$> as)) <> "]"

pStats :: GmState -> Doc ann
pStats s = "Steps taken=" <+> pretty (statGetSteps $ gmStats s)

pDump :: GmState -> Doc ann
pDump s = "  Dump:["
          <> indent 1 (mconcat (punctuate ", " (pDumpItem <$> (reverse (gmDump s)))))
          <+> "]"

pDumpItem :: GmDumpItem -> Doc ann
pDumpItem (code, stack) = "<" <> pShortInstruction 3 code <> "," <+> pShortStack stack <> ">"

pShortInstruction :: Int -> GmCode -> Doc ann
pShortInstruction number code
  = "{" <> mconcat (punctuate "; " dotcodes) <> "}"
    where
      codes = pInstruction <$> take number code
      dotcodes | length code > number = codes <> ["..."]
               | otherwise = codes

pShortStack :: GmStack -> Doc ann
pShortStack stack = "[" <> mconcat (punctuate ", " (pretty . showAddr <$> stack)) <> "]"

pOutput :: GmState -> Doc ann
pOutput s = "Output:\"" <> pretty (gmOutput s) <> "\""
