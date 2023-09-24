{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Ch03.ProcLang.Types where

import Data.Kind (Type)
-- Env
class Environment (e :: Type -> Type) where
  type Var e :: Type
  emptyEnv :: e v
  applyEnv :: e v -> Var e -> v
  extendEnv :: Var e -> v -> e v -> e v
  extendEnvStar :: [Var e] -> [v] -> e v -> e v
  extendEnvStar vars vals env = foldr (uncurry extendEnv) env $ zip vars vals
  removeEnv :: Var e -> e v -> e v
  subsetOfEnv :: [Var e] -> e v -> e v

newtype Lst var val = L [(var,val)]
  deriving (Show, Eq)

instance Environment (Lst String) where
  type Var (Lst String) = String
  emptyEnv = L []
  applyEnv (L []) _ = error "empty env"
  applyEnv (L (x:xs)) var | let (var',val) = x, var == var' = val
                          | otherwise = applyEnv (L xs) var
  extendEnv var val (L env) = L $ (var,val) : env
  removeEnv var (L env) = L $ filter ((/= var) . fst) env
  subsetOfEnv vars (L env) = L $ filter ((`elem` vars) . fst) env

type Env = Lst String Val


newtype Program = Program Exp
  deriving (Show)

type Ident = String

data Exp
  = Const Int
  | Var Ident
  | If Exp Exp Exp
  | Let [ (Ident, Exp) ] Exp
  | ProcExp [ Ident ] Exp
  | ProcDExp [ Ident ] Exp
  | App Exp [ Exp ]
  | Diff Exp Exp
  | Op Ident [ Exp ]
  deriving (Show, Eq)

data Proc = Proc [ Ident ] Exp Env
  deriving (Show, Eq)

data ProcD = ProcD [ Ident ] Exp
  deriving (Show, Eq)

data Val
  = NumVal Int
  | BoolVal Bool
  | ProcVal Proc
  | ProcDVal ProcD
  | ListVal [Val]
  deriving (Show,Eq)

class FromVal a where
  fromVal :: String -> Val -> a

instance FromVal Bool where
  fromVal msg val =
    case val of
      BoolVal b -> b
      o -> error $ msg <> ": " <> show o

instance FromVal Int where
  fromVal msg val =
    case val of
      NumVal i -> i
      o -> error $ msg <> ": " <> show o

instance FromVal [Val] where
  fromVal msg val =
    case val of
      ListVal xs -> xs
      o -> error $ msg <> ": " <> show o

instance FromVal Proc where
  fromVal msg val =
    case val of
      ProcVal p -> p
      o -> error $ msg <> ": " <> show o

instance FromVal ProcD where
  fromVal msg val =
    case val of
      ProcDVal p -> p
      o -> error $ msg <> ": " <> show o

instance FromVal Val where
  fromVal _ = id

class ToVal a where
  toVal :: a -> Val

instance ToVal Bool where
  toVal = BoolVal

instance ToVal Int where
  toVal = NumVal

instance ToVal [Val] where
  toVal = ListVal

instance ToVal Proc where
  toVal = ProcVal

instance ToVal ProcD where
  toVal = ProcDVal

instance ToVal Val where
  toVal = id

-- data ValT a where
--   NumValT :: Int -> ValT Int
--   BoolValT :: Bool -> ValT Bool
--   ProcValT :: Proc -> ValT Proc
--   ListValT :: [Val] -> ValT [Val]
