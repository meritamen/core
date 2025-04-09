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

pState :: GmState -> Doc ann
pState s = pStack s <> line <> pInstructions (gmCode s) <> line

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

pStats :: GmState -> Doc ann
pStats s = "Steps taken=" <+> pretty (statGetSteps (gmStats s))
