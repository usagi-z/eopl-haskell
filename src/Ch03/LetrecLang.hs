{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Ch03.LetrecLang where

import Ch03.LetrecLang.Types
import Ch03.LetrecLang.Parsing
import Ch03.LetrecLang.Operators
import Data.Set (Set(..), singleton, (\\))
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad (foldM)

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
      let env' = subsetOfEnv
                   (Set.toList $ freeVars body \\ Set.fromList vars) env
      in return $ ProcVal $ Proc vars body env'
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
