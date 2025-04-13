{-# LANGUAGE OverloadedStrings #-}

module Core.LambdaLifting (lambdaLift) where

import           Data.List (mapAccumL)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Set  (Set)
import qualified Data.Set  as Set

import Core.Language
import Core.Utils

type AnnExpr a b = (b, AnnExpr' a b)

data AnnExpr' a b = AVar Name
                  | ANum Int
                  | AConstr Int Int
                  | AAp (AnnExpr a b) (AnnExpr a b)
                  | ALet Bool [AnnDefn a b] (AnnExpr a b)
                  | ACase (AnnExpr a b) [AnnAlt a b]
                  | ALam [a] (AnnExpr a b)

type AnnDefn a b = (a, AnnExpr a b)

type AnnAlt a b = (Int, [a], AnnExpr a b)

type AnnProgram a b = [(Name, [a], AnnExpr a b)]

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars

freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = [ (name, args, freeVars_e (Set.fromList args) body )
                | (name, args, body) <- prog
                ]

freeVars_e :: Set Name -> CoreExpr -> AnnExpr Name (Set Name)
freeVars_e _ (ENum k) = (Set.empty, ANum k)
freeVars_e lv (EVar v) | Set.member v lv = (Set.singleton v, AVar v)
                       | otherwise = (Set.empty, AVar v)
freeVars_e lv (EAp e1 e2) = (freeVarsOf e1' `Set.union` freeVarsOf e2', AAp e1' e2')
  where
    e1' = freeVars_e lv e1
    e2' = freeVars_e lv e2
freeVars_e lv (ELam args body) = (freeVarsOf body' Set.\\ Set.fromList args, ALam args body')
  where
    body' = freeVars_e new_lv body
    new_lv = lv `Set.union` Set.fromList args
freeVars_e lv (ELet recursive defns body)
  = (defnsFree `Set.union` bodyFree, ALet recursive defns' body')
  where
    binders = bindersOf defns
    binderSet = Set.fromList binders
    body_lv = lv `Set.union` binderSet
    rhss_lv | recursive = body_lv | otherwise = lv
    rhss' = freeVars_e rhss_lv <$> rhssOf defns
    defns' = zip binders rhss'
    freeInValues = Set.unions $ freeVarsOf <$> rhss'
    defnsFree | recursive = freeInValues Set.\\ binderSet
              | otherwise = freeInValues
    body' = freeVars_e body_lv body
    bodyFree = freeVarsOf body' Set.\\ binderSet
freeVars_e _ (EConstr t a) = (Set.empty, AConstr t a)
freeVars_e lv (ECase e alts) = (freeVarsOf e' `Set.union` free, ACase e' alts')
  where
    e' = freeVars_e lv e
    alts' = [ (tag, args, freeVars_e (lv `Set.union` Set.fromList args) e)
            | (tag, args, e) <- alts]
    free = Set.unions $ freeVarOf_alt <$> alts'

freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (free_vars, _) = free_vars

freeVarOf_alt :: AnnAlt Name (Set Name) -> Set Name
freeVarOf_alt (_, args, rhs) = freeVarsOf rhs Set.\\ Set.fromList args

abstract :: AnnProgram Name (Set Name) -> CoreProgram
abstract prog = [ (sc_name, args, abstract_e rhs)
                | (sc_name, args, rhs) <- prog
                ]

abstract_e :: AnnExpr Name (Set Name) -> CoreExpr
abstract_e (_, AVar v) = EVar v
abstract_e (_, ANum k) = ENum k
abstract_e (_, AAp e1 e2) = EAp (abstract_e e1) (abstract_e e2)
abstract_e (_, ALet recursive defns body)
  = ELet recursive [ (name, abstract_e body) | (name, body) <- defns ]
                     (abstract_e body)
abstract_e (free, ALam args body) = foldl EAp sc $ EVar <$> fvList
  where
    fvList = Set.toList free
    sc = ELet False [("sc", sc_rhs)] $ EVar "sc"
    sc_rhs = ELam (fvList <> args) $ abstract_e body
abstract_e (_, AConstr t a) = EConstr t a
abstract_e (_, ACase e alts) =
  ECase (abstract_e e) [(tag, args, abstract_e e) | (tag, args, e) <- alts]

rename :: CoreProgram -> CoreProgram
rename prog = snd (mapAccumL rename_sc initialNameSupply prog)
  where
    rename_sc ns (sc_name, args, rhs)
      = (ns2, (sc_name, args', rhs'))
        where
          (ns1, args', env) = newNames ns args
          (ns2, rhs') = rename_e env ns1 rhs

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], Map Name Name)
newNames ns old_names = (ns', new_names, env)
  where
    (ns', new_names) = getNames ns old_names
    env = Map.fromList $ zip old_names new_names

rename_e ::  Map Name Name -> NameSupply -> CoreExpr -> (NameSupply, CoreExpr)
rename_e env ns (EVar v) = (ns, EVar $ f v env)
  where
    f var env = case Map.lookup var env of
                  Just var' -> var'
                  Nothing   -> var
rename_e _   ns (ENum n) = (ns, ENum n)
rename_e env ns (EAp e1 e2) = (ns2, EAp e1' e2')
  where
    (ns1, e1') = rename_e env ns e1
    (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body) = (ns1, ELam args' body')
  where
    (ns1, args', env') = newNames ns args
    (_, body') = rename_e (env' `Map.union` env) ns1 body
rename_e env ns (ELet is_rec defns body)
  = (ns3, ELet is_rec (zip binders' rhss') body')
  where
    (ns1, body') = rename_e body_env ns body
    binders = bindersOf defns
    (ns2, binders', env') = newNames ns1 binders
    body_env = env' `Map.union` env
    (ns3, rhss') = mapAccumL (rename_e rhsEnv) ns2 (rhssOf defns)
    rhsEnv | is_rec = body_env
           | otherwise = env
rename_e _ ns (EConstr t a) = (ns, EConstr t a)
rename_e env ns (ECase e alts) = (ns2, ECase e' alts')
  where
    (_, e') = rename_e env ns e
    (ns2, alts') = mapAccumL rename_alt ns alts
    rename_alt ns (tag, args, rhs) = (ns2, (tag, args', rhs'))
      where
        (ns1, args', env') = newNames ns args
        (ns2, rhs') = rename_e (env' `Map.union` env) ns1 rhs

collectSCs :: CoreProgram -> CoreProgram
collectSCs prog = mconcat (collect_one_sc <$> prog)
  where
    collect_one_sc (sc_name, args, rhs)
      = (sc_name, args, rhs') : scs
        where
          (scs, rhs') = collectSCs_e rhs

collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCs_e (ENum k) = ([], ENum k)
collectSCs_e (EVar v) = ([], EVar v)
collectSCs_e (EAp e1 e2) = (scs1 <> scs2, EAp e1' e2')
  where
    (scs1, e1') = collectSCs_e e1
    (scs2, e2') = collectSCs_e e2
collectSCs_e (ELam args body) = (scs, ELam args body')
  where
    (scs, body') = collectSCs_e body
collectSCs_e (EConstr t a) = ([], EConstr t a)
collectSCs_e (ECase e alts) = (scs_e <> scs_alts, ECase e' alts')
  where
    (scs_e, e') = collectSCs_e e
    (scs_alts, alts') = mapAccumL collectSCs_alt [] alts
    collectSCs_alt scs (tag, args, rhs) = (scs <> scs_rhs, (tag, args, rhs'))
      where
        (scs_rhs, rhs') = collectSCs_e rhs
collectSCs_e (ELet is_rec defns body)
  = (rhss_scs ++ body_scs ++ local_scs, mkELet is_rec non_scs' body')
    where
      (rhss_scs,defns') = mapAccumL collectSCs_d [] defns
      scs' = [(name,rhs) | (name,rhs) <- defns', isELam rhs ]
      non_scs' = [(name,rhs) | (name,rhs) <- defns', not (isELam rhs)]
      local_scs = [(name,args,body) | (name, ELam args body) <- scs']
      (body_scs, body') = collectSCs_e body
      collectSCs_d scs (name,rhs) = (scs <> rhs_scs, (name, rhs'))
        where
          (rhs_scs, rhs') = collectSCs_e rhs

isELam :: Expr a -> Bool
isELam (ELam _ _) = True
isELam _          = False

mkELet :: IsRec -> [(a, Expr a)] -> Expr a -> Expr a
mkELet is_rec defns body = ELet is_rec defns body

bindersOf :: [(a, b)] -> [a]
bindersOf = (fst <$>)

rhssOf :: [(a, b)] -> [b]
rhssOf = (snd <$>)
