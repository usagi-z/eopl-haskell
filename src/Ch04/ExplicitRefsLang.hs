
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
import qualified Data.Set as Set

initEnv :: Env
initEnv = extendEnvStar
            ["i", "v", "x"]
            (fmap NumVal [1,5,10])
            emptyEnv

-- procedures
type Eval = Writer [String] Val

applyProcedure :: Proc -> [ Val ] -> Eval
applyProcedure (Proc is body env) vs =
  valueOf body $ extendEnvStar is vs env

-- evaluation
valueOf :: Exp -> Env -> Writer [String] Val
valueOf exp env = do
  tell [ showTrace exp ]
  case exp of
    Const n -> return $ NumVal n
    Var ident -> return $ applyEnv env ident
    Diff m s -> do
      m' <- valueOf m env
      s' <- valueOf s env
      case (m', s') of
        (NumVal m', NumVal s') -> return $ NumVal $ m' - s'
        (m',s') -> error $ "Diff type error: minuend - " <> show m' <>
                           ", subtrahend - " <> show s'
    Op name xs ->
      case lookup name operators of
        Nothing -> error $ "operator " <> name <> " not defined"
        Just f -> do args <- mapM (`valueOf` env) xs
                     return $ f args
    If c t e -> do
      val <- valueOf c env
      let cb = fromVal @Bool "If condition: " val
      if cb then valueOf t env else valueOf e env

    -- let* in the book
    Let defs b -> do
      let f env (ident, defExp) = do
            v <- valueOf defExp env
            return $ extendEnv ident v env
      env' <- foldM f env defs
      valueOf b env'
    Letrec defs letBody ->
      valueOf letBody $ ExtendEnvRec defs env
    ProcExp vars body ->
      return $ ProcVal $ Proc vars body env
    App rator rands -> do
      rator' <- valueOf rator env
      case rator' of
        ProcVal p -> do
          rands' <- mapM (`valueOf` env) rands
          applyProcedure p rands'
        _ -> error "App: rator is not a procedure"

valueOfProgram :: Program -> Val
valueOfProgram (Program e) = fst $ runWriter $ valueOf e initEnv

traceProgram' :: Program -> [ String ]
traceProgram' (Program e) = execWriter $ valueOf e initEnv

trace :: String -> Either String [String]
trace = fmap traceProgram' . scanAndParse

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
