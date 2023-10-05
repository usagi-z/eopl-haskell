
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Ch04.ExplicitRefsLang where

import Ch04.ExplicitRefsLang.Parsing
import Ch04.ExplicitRefsLang.Types
import Ch04.ExplicitRefsLang.Operators
import Data.Function (on)
import Data.List (find)
import Data.Foldable (foldrM, foldlM)
import Parsing2
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Data.Proxy (Proxy(..))
import Data.Set (Set(..), singleton, (\\))
import Control.Monad.Writer
import Control.Monad (foldM)
import Control.Monad.RWS.Strict
import qualified Data.Set as Set
import Control.Monad.Identity (Identity)

initEnv :: Env
initEnv = extendEnvStar
            ["i", "v", "x"]
            (fmap NumVal [1,5,10])
            emptyEnv

-- procedures
type Store = [Val]
-- type Eval = Writer [String] Val
type Eval = RWS Env [String] Store Val

applyProcedure :: Proc -> [ Val ] -> Eval
applyProcedure (Proc is body env) vs =
  local (const $ extendEnvStar is vs env) $ valueOf body

-- evaluation
valueOf :: Exp -> Eval
valueOf exp = do
  tell [ showTrace exp ]
  case exp of
    Const n -> return $ NumVal n
    Var ident -> do
      env <- ask
      return $ applyEnv env ident
    Diff m s -> do
      m' <- valueOf m
      s' <- valueOf s
      case (m', s') of
        (NumVal m', NumVal s') -> return $ NumVal $ m' - s'
        (m',s') -> error $ "Diff type error: minuend - " <> show m' <>
                           ", subtrahend - " <> show s'
    Op name xs ->
      case lookup name operators of
        Nothing -> error $ "operator " <> name <> " not defined"
        Just f -> do args <- mapM valueOf xs
                     return $ f args
    If c t e -> do
      val <- valueOf c
      let cb = fromVal @Bool "If condition: " val
      if cb then valueOf t else valueOf e

    -- let* in the book
    Let defs b -> do
      let f env (ident, defExp) = do
            v <- local (const env) $ valueOf defExp
            return $ extendEnv ident v env
      env <- ask
      env' <- foldM f env defs
      local (const env') $ valueOf b
    Letrec defs letBody ->
      local (ExtendEnvRec defs) $ valueOf letBody
    ProcExp vars body -> do
      env <- ask
      return $ ProcVal $ Proc vars body env
    App rator rands -> do
      rator' <- valueOf rator
      case rator' of
        ProcVal p -> do
          rands' <- mapM valueOf rands
          applyProcedure p rands'
        _ -> error "App: rator is not a procedure"

valueOfProgram :: Program -> Val
valueOfProgram (Program e) =
  let (val,_store, _trace) = runRWS (valueOf e) initEnv []
  in val

-- traceProgram' :: Program -> [ String ]
-- traceProgram' (Program e) = execWriter $ valueOf e initEnv

-- trace :: String -> Either String [String]
-- trace = fmap traceProgram' . scanAndParse

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
