module Ch04.ImplicitRefsLang where

import Ch04.ImplicitRefsLang.Parsing
import Ch04.ImplicitRefsLang.Types
import Ch04.ImplicitRefsLang.Operators
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
import Data.Traversable (for)


type Eval a = RWS Env [String] Store a

applyEnv :: Env -> Ident -> Eval Int
applyEnv EmptyEnv _ = error "empty env"
applyEnv (ExtendEnv var' val env) var
  | var == var' = return val
  | otherwise = applyEnv env var
applyEnv env@(ExtendEnvRec defs env') var =
  case findProc var defs of
    Nothing -> applyEnv env' var
    Just (name, args, body) -> do
      newref $ ProcVal $ Proc args body env

newref :: Val -> Eval Int
newref v = do
  modify (|> v)
  store <- get
  let newref = S.length store - 1
  return newref

applyProcedure :: Proc -> [ Val ] -> Eval Val
applyProcedure (Proc is body env) vs = do
  newrefs <- for vs newref
  local (const $ extendEnvStar is newrefs env) $ valueOf body

valueOf :: Exp -> Eval Val
valueOf exp = do
  case exp of
    Const n -> return $ NumVal n
    Var ident -> do
      env <- ask
      store <- get
      ref <- applyEnv env ident
      return $ case deref ref store of
                 Nothing -> error $ "var " <> ident <> " is a dangling reference"
                 Just val -> val
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
    Let defs b -> do
      let f env (ident, defExp) = do
            v <- local (const env) $ valueOf defExp
            ref <- newref v
            return $ extendEnv ident ref env
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
    SetExp ident exp -> do
      val <- valueOf exp
      env <- ask
      ref <- applyEnv env ident
      modify $ S.update ref val
      return $ NumVal 888
    BeginExp exps -> do
      let evalSeq [] = error "no statements in begin"
          evalSeq [x] = valueOf x
          evalSeq (x:xs) = do
            _ <- valueOf x
            evalSeq xs
      evalSeq exps
