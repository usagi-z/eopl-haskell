{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Ch03.LexAddrLang where

import Ch03.LexAddrLang.Types
import Ch03.LexAddrLang.Parsing
import Ch03.LexAddrLang.Operators
import Ch03.LexAddrLang.Environment
import Data.Set (Set(..), singleton, (\\))
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad (foldM)

initEnv :: Env
initEnv = fmap NumVal [1,5,10]

initSEnv :: SEnv
initSEnv = fmap NonRec ["i", "v", "x"]

type Eval = Writer [String] Val

translationOf :: Exp -> SEnv -> Exp
translationOf exp senv =
  case exp of
    c@(Const _) -> c
    (Diff x y) -> Diff (translationOf x senv) (translationOf y senv)
    (If c t e) -> If (translationOf c senv)
                     (translationOf t senv)
                     (translationOf e senv)
    (Op ident rands) -> Op ident (fmap (`translationOf` senv) rands)
    (App rator rands) -> App (translationOf rator senv)
                             (fmap (`translationOf` senv) rands)

    (Var ident) -> let (i, rec) = applySEnv ident senv
                   in if rec
                      then NLetrecVar i
                      else NVar i
    (Let var def body) ->
      NLet (translationOf def senv)
           (translationOf body
             (extendSEnv (NonRec var) senv))
    (Letrec pName pVar pBody letBody) ->
      NLetrec (translationOf pBody
               (extendSEnv (NonRec pVar) $ extendSEnv (Rec pName) senv))
              (translationOf letBody (extendSEnv (Rec pName) senv))
    (ProcExp vars body) ->
      NProc $ translationOf body (fmap NonRec vars <> senv)
    _ -> error "translationOf: invalid source expression"


applyProcedure :: Proc -> [ Val ] -> Eval
applyProcedure (Proc body env) vs =
  valueOf body $ extendEnvStar vs env

valueOf :: Exp -> Env -> Writer [String] Val
valueOf exp env = do
  -- tell [ showTrace exp ]
  case exp of
    Const n -> return $ NumVal n
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
    App rator rands -> do
      rator' <- valueOf rator env
      case rator' of
        ProcVal p -> do
          rands' <- mapM (`valueOf` env) rands
          applyProcedure p rands'
        _ -> error "App: rator is not a procedure"
    NVar i -> pure $ applyEnv env i
    NLetrecVar i -> do
      let v = applyEnv env i
          (Proc exp env') = fromVal @Proc "letrec-var" v
      pure $ ProcVal $ Proc exp (extendEnv v env')
    NLet def body -> do
      vdef <- valueOf def env
      valueOf body $ extendEnv vdef env
    NLetrec procBody letBody ->
      let env' = extendEnv (ProcVal $ Proc procBody env) env
      in valueOf letBody env'
    NProc body -> pure $ ProcVal $ Proc body env
    _ -> error "valueOf: invalid translated expression"

valueOfProgram :: Program -> Val
valueOfProgram (Program e) =
  fst $ runWriter $ valueOf (translationOf e initSEnv) initEnv

-- traceProgram' :: Program -> [ String ]
-- traceProgram' (Program e) = execWriter $ valueOf e initEnv

-- trace :: String -> Either String [String]
-- trace = fmap traceProgram' . scanAndParse

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
