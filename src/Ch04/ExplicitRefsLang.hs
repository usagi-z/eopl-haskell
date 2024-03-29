
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
import Data.Foldable (foldrM, foldlM, forM_)
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
import Data.List.Extra ((!?))
import Data.Sequence (Seq(..), update, (|>))
import qualified Data.Sequence as S

initEnv :: Env
initEnv = extendEnvStar
            ["i", "v", "x"]
            (fmap NumVal [1,5,10])
            emptyEnv

-- procedures
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
      case Prelude.lookup name operators of
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
    NewrefExp exp -> do
      val <- valueOf exp
      modify (|> val)
      store <- get
      return $ RefVal $ Ref (S.length store - 1)
    DerefExp exp -> do
      val <- valueOf exp
      let ref = fromVal @Ref "deref" val
          Ref i = ref
      store <- get
      let valMb = deref i store
      case valMb of
        Nothing -> error $ "invalid store reference " <> show i
        Just val -> return val
    SetrefExp refExp valExp -> do
      ref <- valueOf refExp
      let Ref i = fromVal @Ref "setref" ref
      val <- valueOf valExp
      modify $ S.update i val
      return (NumVal 23)
    BeginExp exps -> do
      let evalSeq [] = error "no statements in begin"
          evalSeq [x] = valueOf x
          evalSeq (x:xs) = do
            _ <- valueOf x
            evalSeq xs
      evalSeq exps


valueOfProgram :: Program -> Val
valueOfProgram (Program e) =
  let (val,_store, _trace) = runRWS (valueOf e) initEnv emptyStore
  in val

-- traceProgram' :: Program -> [ String ]
-- traceProgram' (Program e) = execWriter $ valueOf e initEnv

-- trace :: String -> Either String [String]
-- trace = fmap traceProgram' . scanAndParse

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
