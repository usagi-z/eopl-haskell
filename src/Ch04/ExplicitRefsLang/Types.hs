{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Ch04.ExplicitRefsLang.Types where

import Data.Kind (Type)
import Data.Foldable (find)
-- Env

data Env
  = EmptyEnv
  | ExtendEnv Ident Val Env
  | ExtendEnvRec [ ( Ident, [ Ident ], Exp ) ] Env
  deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = EmptyEnv

fstOfTriple (x,_,_) = x
findProc :: Ident -> [ ( Ident, [ Ident ], Exp ) ] -> Maybe ( Ident, [ Ident ], Exp )
findProc name = find matches
  where matches = (== name) . fstOfTriple

applyEnv :: Env -> Ident -> Val
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

extendEnvStar :: [Ident] -> [Val] -> Env -> Env
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
  | NewrefExp Exp
  | DerefExp Exp
  | SetrefExp Exp Exp
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

data Proc = Proc [ Ident ] Exp Env
  deriving (Show, Eq)

data Val
  = NumVal Int
  | BoolVal Bool
  | ProcVal Proc
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

instance ToVal Val where
  toVal = id

-- data ValT a where
--   NumValT :: Int -> ValT Int
--   BoolValT :: Bool -> ValT Bool
--   ProcValT :: Proc -> ValT Proc
--   ListValT :: [Val] -> ValT [Val]
