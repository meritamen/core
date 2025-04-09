{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Compiler where

import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Core.AST
import Language.Core.Machine
import Language.Core.Prelude
import Language.Core.Utils

type GmCompiledSC = (Name, Int, GmCode)
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = Map Name Int

compile :: CoreProgram -> GmState
compile program = GmState initialCode [] heap globals statInitial
  where
    (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = Map.fromList <$>  mapAccumL allocateSc Map.empty compiled
  where
    compiled = compiledPrimitives <> (compileSc <$> (preludeDefs <> program))

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where
    (heap', addr) = alloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body $ Map.fromList (zip env [0..]))

compileR :: GmCompiler
compileR e env = compileC e env <> [Slide (length env +1), Unwind]

compileC :: GmCompiler
compileC (EVar v) env
  | elem v (fst <$> Map.toList env) = [Push $ env Map.! v]
  | otherwise = [Pushglobal v]
compileC (ENum n) _ = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env <> compileC e1 (argOffset 1 env) <> [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = (+n) <$> env

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []
