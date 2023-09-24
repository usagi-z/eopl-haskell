{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Ch03.ProcLang where

import Ch03.ProcLang.Types
import Ch03.ProcLang.Parsing
import Ch03.ProcLang.Operators4
import Data.Function (on)
import Data.List (find)
import Data.Foldable (foldrM, foldlM)
import Parsing2
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Data.Proxy (Proxy(..))
import Data.Set (Set(..), singleton, (\\))
import Control.Monad (join)
import qualified Data.Set as Set

initEnv :: Env
initEnv = extendEnvStar
            ["i", "v", "x"]
            (fmap NumVal [1,5,10])
            emptyEnv

-- procedures

applyProcedure :: Proc -> [ Val ] -> Val
applyProcedure (Proc is body env) vs =
  valueOf body $ extendEnvStar is vs env

applyProcedureD :: ProcD -> Env -> [ Val ] -> Val
applyProcedureD (ProcD is body) env vs =
  valueOf body $ extendEnvStar is vs env



freeVars :: Exp -> Set Ident
freeVars = \case
  Var ident -> Set.singleton ident
  Const _ -> Set.empty
  If c t e -> freeVars c <> freeVars t <> freeVars e
  Let defs body ->
    let freeInDefs = snd $ foldl f (Set.empty, Set.empty) defs
        f (soFar, free) (ident, def) =
          let free' = freeVars def \\ soFar
          in (Set.insert ident soFar, Set.union free free')
    in Set.union freeInDefs
                (freeVars body \\ Set.fromList (fmap fst defs))
  ProcExp ids body -> freeVars body \\ Set.fromList ids
  Op _ exps -> foldMap freeVars exps
  App rator rands -> freeVars rator <> foldMap freeVars rands
  Diff x y -> freeVars x <> freeVars y

freeVarsOfProgram :: String -> Either String (Set Ident)
freeVarsOfProgram =  fmap (freeVars . o) . scanAndParse
  where
    o (Program exp) = exp

-- evaluation
valueOf :: Exp -> Env -> Val
valueOf exp env =
  case exp of
    Const n -> NumVal n
    Var ident -> applyEnv env ident
    Diff m s ->
      case (valueOf m env , valueOf s env) of
        (NumVal m', NumVal s') -> NumVal $ m' - s'
        (m',s') -> error $ "Diff type error: minuend - " <> show m' <>
                           ", subtrahend - " <> show s'
    Op name xs ->
      case lookup name operators of
        Nothing -> error $ "operator " <> name <> " not defined"
        Just f -> f $ fmap (`valueOf` env) xs

    If c t e ->
      let val = valueOf c env
          cb = fromVal @Bool "If condition: " val
      in if cb then valueOf t env else valueOf e env

    -- let* in the book
    Let defs b ->
      let f env (ident, defExp) =
            extendEnv ident (valueOf defExp env) env
          env' = foldl f env defs
      in valueOf b env'

    ProcExp vars body ->
      let env' = subsetOfEnv
                   (Set.toList $ freeVars body \\ Set.fromList vars) env
      in ProcVal $ Proc vars body env'
    ProcDExp vars body ->
      ProcDVal $ ProcD vars body
    App rator rands ->
      case valueOf rator env of
        ProcVal p -> applyProcedure p $ fmap (`valueOf` env) rands
        ProcDVal pd -> applyProcedureD pd env $ fmap (`valueOf` env) rands
        _ -> error "App: rator is not a procedure"

valueOfProgram :: Program -> Val
valueOfProgram (Program e) = valueOf e initEnv

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
