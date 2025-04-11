{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Prettyprint where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc, pretty, (<+>), punctuate, indent, line, vsep)

import Language.Core.Machine
import Language.Core.Utils

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
  <> "}" <> line

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
  Cond l r -> "Cond" <+> "[" <+> mconcat (punctuate ", " (pInstruction <$> l)) <+> "]"
                     <+> "[" <+> mconcat (punctuate ", " (pInstruction <$> r)) <+> "]"
  Pack a1 a2 -> "Pack" <+> pretty a1 <+> pretty a2
  Casejump alts ->
    "Casejump"
    <+> "["
    <+> mconcat ((\(n, c) -> pretty n <> "->"
                             <+> "["
                             <+> mconcat (punctuate ", " (pInstruction <$> c))
                             <+> "]") <$> alts)
    <+> "}]"
  Split n -> "Split" <+> pretty n
  Print -> "Print"

pState :: GmState -> Doc ann
pState s = pOutput s <> line <> pStack s <> line <> pDump s <> line <> pInstructions (gmCode s) <> line

pStack :: GmState -> Doc ann
pStack s =
  " Stack:["
  <> indent 1 (mconcat (punctuate ", " (pStackItem s <$> gmStack s)))
  <> "]"

pStackItem :: GmState -> Addr -> Doc ann
pStackItem s a = pretty (showAddr a) <> ":" <+> pNode s a (gmHeap s Map.! a)

pNode :: GmState -> Addr -> Node -> Doc ann
pNode _ _ (NNum n) = pretty n
pNode s a (NGlobal _ _) = "Global" <+> pretty (head [n | (n, b) <- Map.toList $ gmGlobals s, a == b])
pNode _ _ (NAp a1 a2) = "Ap" <+> pretty (showAddr a1) <+> pretty (showAddr a2)
pNode _ _ (NInd a) = "Ind" <+> pretty (showAddr a)
pNode _ _ (NConstr t as) =
  "Cons "
  <> pretty t <+> "["
  <> mconcat (punctuate "," (pretty . showAddr <$> as))
  <> "]"

pStats :: GmState -> Doc ann
pStats s = "Steps taken=" <+> pretty (statGetSteps $ gmStats s)

pDump :: GmState -> Doc ann
pDump s =
  "  Dump:["
  <> indent 1 (mconcat (punctuate ", " (pDumpItem <$> (reverse (gmDump s)))))
  <> "]"

pDumpItem :: GmDumpItem -> Doc ann
pDumpItem (code, stack) = "<" <> pShortInstructions 3 code <+> "," <> pShortStack stack <> ">"

pShortInstructions :: Int -> GmCode -> Doc ann
pShortInstructions number code
  = "{" <> mconcat (punctuate "; " dotcodes) <> "}"
    where
      codes = pInstruction <$> take number code
      dotcodes | length code > number = codes <> ["..."]
               | otherwise = codes

pShortStack :: GmStack -> Doc ann
pShortStack stack = "[" <> mconcat (punctuate ", " ((pretty . showAddr) <$> stack)) <> "]"

pOutput :: GmState -> Doc ann
pOutput s = "Output:\"" <> pretty (gmOutput s) <> "\""
