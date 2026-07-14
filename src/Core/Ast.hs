{-# LANGUAGE StrictData #-}

module Core.Ast where

import Core.Utils
import Data.Text (Text)

type IsRec = Bool

type CoreExpr = Expr Name

type CoreAlt = Alter Name

type CoreProgram = Program Name

type CoreScDefn = ScDefn Name

data Expr a
  = EVar Text
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

type Alter a = (Int, [a], Expr a)

type Program a = [ScDefn a]

type ScDefn a = (Name, [a], Expr a)
