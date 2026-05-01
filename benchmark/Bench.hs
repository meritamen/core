{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Criterion.Main

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import GHC.Generics

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Core.Compiler
import Core.Language
import Core.LambdaLifting
import Core.Machine
import Core.Parser

main :: IO ()
main = defaultMain benchmarks
  where
    benchmarks = bench_parse <> bench_compile <> bench_evaluate

deriving instance Generic CoreExpr
deriving instance NFData CoreExpr

deriving instance Generic GmState
deriving instance NFData GmState

deriving instance Generic Instruction
deriving instance NFData Instruction

deriving instance Generic Node
deriving instance NFData Node

readAllCoreFiles :: FilePath -> IO [(FilePath, T.Text)]
readAllCoreFiles dir = do
    files <- listDirectory dir
    let fullPaths = map (dir </>) files
    txtFiles <- filterM (\fp -> do
        isFile <- doesFileExist fp
        pure (isFile && takeExtension fp == ".core")) fullPaths
    forM txtFiles $ \fp -> do
        content <- TIO.readFile fp
        pure (takeFileName fp, content)

coreFiles :: [(FilePath, Text)]
coreFiles = unsafePerformIO $ readAllCoreFiles "/Users/meritamen/Projects/core/test/examplePrograms/"

bench_parse :: [Benchmark]
bench_parse = [
  bgroup "parse Core files" $
    map (\(desc, prog) ->
           bench desc $ nf parseCore prog)
        coreFiles
  ]

bench_compile :: [Benchmark]
bench_compile = [
  bgroup "compile Core files" $
    map (\(desc, prog) ->
           bench desc $ nf (compile . lambdaLift . parseCore) prog)
        coreFiles
  ]

bench_evaluate :: [Benchmark]
bench_evaluate = [
  bgroup "evaluate Core files" $
    map (\(desc, prog) ->
           bench desc $ nf (evalState eval . compile . lambdaLift . parseCore) prog)
        coreFiles
  ]
