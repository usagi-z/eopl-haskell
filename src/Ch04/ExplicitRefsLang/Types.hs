{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Ch04.ExplicitRefsLang.Types where

import Data.Kind (Type)
import Data.Foldable (find)
-- Env

data Env a where
  EmptyEnv :: Env a
  ExtendEnv :: Ident -> (Val a) -> Env a -> Env a
  ExtendEnvRec :: [ ( Ident, [ Ident ], Exp ) ] -> Env a -> Env a
  -- deriving (Show)

emptyEnv :: Env a
emptyEnv = EmptyEnv

fstOfTriple (x,_,_) = x
findProc :: Ident -> [ ( Ident, [ Ident ], Exp ) ] -> Maybe ( Ident, [ Ident ], Exp )
findProc name = find matches
  where matches = (== name) . fstOfTriple

applyEnv :: Env a -> Ident -> Val a
applyEnv EmptyEnv _ = error "empty env"
applyEnv (ExtendEnv var' val env) var
  | var == var' = val
  | otherwise = applyEnv env var
applyEnv env@(ExtendEnvRec defs env') var =
  case findProc var defs of
    Nothing -> applyEnv env' var
    Just (name, args, body) -> ProcVal $ Proc args body env

extendEnv = ExtendEnv

extendEnvRec = ExtendEnvRec

extendEnvStar :: [Ident] -> [Val a] -> Env a -> Env a
extendEnvStar ids vals env = foldr (uncurry extendEnv) env $ zip ids vals

newtype Program = Program Exp
  deriving (Show)

type Ident = String

data Exp
  = Const Int
  | Var Ident
  | If Exp Exp Exp
  | Let [ (Ident, Exp) ] Exp
  | Letrec [ (Ident, [ Ident ], Exp) ] Exp
  | ProcExp [ Ident ] Exp
  | App Exp [ Exp ]
  | Diff Exp Exp
  | Op Ident [ Exp ]
  deriving (Show, Eq)

showTrace :: Exp -> String
showTrace c@(Const _) = show c
showTrace (If {}) = "if"
showTrace (Let defs _) = let ds = unwords $ fmap fst defs
                         in "let " <> ds
showTrace (Letrec defs _) = let ds = unwords $ fmap (\(x,_,_) -> x) defs
                            in "letrec " <> ds
showTrace (ProcExp vars _) = "proc " <> unwords vars <> " = ..."
showTrace (App (Var name) _) = "app " <> name
showTrace (App _ _) = "app anon"
showTrace (Diff _ _) = "diff"
showTrace (Op name _) = "op " <> name
showTrace (Var name) = "var " <> name

data Proc a = Proc [ Ident ] Exp (Env a)
  -- deriving (Show, Eq)

data Val a where
  NumVal :: Int -> Val Int
  BoolVal :: Bool -> Val Bool
  ProcVal :: Proc a -> Val (Proc a)
  ListVal :: [Val a] -> Val [Val a]

  -- deriving (Show)


fromVal :: Val a -> a
fromVal (NumVal i) = i
fromVal (BoolVal b) = b
fromVal (ProcVal p) = p
fromVal (ListVal vals) = vals
