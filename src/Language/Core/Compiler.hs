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
compile program = GmState initialCode [] [] heap globals statInitial
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
initialCode = [Pushglobal "main", Eval]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body $ Map.fromList (zip env [0..]))

compileR :: GmCompiler
compileR e env = compileE e env <> [Update n, Pop n, Unwind]
  where
    n = length env

compileE :: GmCompiler
compileE (ENum i) _ = [Pushint i]
compileE (ELet False defs e) env = compileLet compileE defs e env
compileE (ELet True defs e) env = compileLetrec compileE defs e env
compileE (EAp (EAp (EVar f) e1) e2) env
  | f `elem` (Map.keys builtInDyadic) = compileE e1 env <> compileE e2 env <> [builtInDyadic Map.! f]
compileE (EAp (EVar "negate") e) env = compileE e env <> [Neg]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env =
  compileE e1 env <> [Cond (compileE e2 env) (compileE e3 env)]
compileE e env = compileC e env <> [Eval]

compileC :: GmCompiler
compileC (EVar v) args
  | elem v (fst <$> Map.toList args) = [Push $ args Map.! v]
  | otherwise = [Pushglobal v]
compileC (ENum n) _ = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env <> compileC e1 (argOffset 1 env) <> [Mkap]
compileC (ELet False defs e) args = compileLet compileC defs e args
compileC (ELet True defs e) args = compileLetrec compileC defs e args

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = (+n) <$> env

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env <> comp expr env' <> [Slide (length defs)]
    where
      env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' [] _ = []
compileLet' ((_, expr):defs) env = compileC expr env <> compileLet' defs (argOffset 1 env)

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr env
  = [Alloc n] <> compileLetrec' (n-1) defs env' <> comp expr env' <> [Slide n]
    where
      n = length defs
      env' = compileArgs defs env

compileLetrec' :: Int -> [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetrec' _ [] _ = []
compileLetrec' n ((_, expr):defs) env = compileC expr env <> [Update n] <> compileLetrec' (n-1) defs env

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = Map.fromList (zip (fst <$> defs) [n-1,n-2..0]) `Map.union` argOffset n env
  where
    n = length defs

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
                     , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
                     , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
                     , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
                     , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
                     , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
                     , ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
                     , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
                     , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
                     , (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
                     , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
                     , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])]

builtInDyadic :: Map Name Instruction
builtInDyadic = Map.fromList [("+", Add), ("-", Sub), ("*", Mul), ("div", Div)
                             , ("==", Eq), ("~=", Ne), (">=", Ge)
                             , (">", Gt), ("<=", Le), ("<", Lt)]
