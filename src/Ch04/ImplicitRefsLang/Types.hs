{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Ch04.ImplicitRefsLang.Types where

import Data.Kind (Type)
import Data.Foldable (find)
import qualified Data.Sequence as S

----- ENVIRONMENT
data Env
  = EmptyEnv
  | ExtendEnv Ident Int Env
  | ExtendEnvRec [ ( Ident, [ Ident ], Exp ) ] Env
  deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = EmptyEnv

fstOfTriple (x,_,_) = x
findProc :: Ident -> [ ( Ident, [ Ident ], Exp ) ] -> Maybe ( Ident, [ Ident ], Exp )
findProc name = find matches
  where matches = (== name) . fstOfTriple

extendEnv = ExtendEnv

-- extendEnvRec = ExtendEnvRec

extendEnvStar :: [Ident] -> [Int] -> Env -> Env
extendEnvStar ids vals env = foldr (uncurry extendEnv) env $ zip ids vals


----- STORE

type Store = S.Seq Val

emptyStore :: Store
emptyStore = mempty

deref :: Int -> Store -> Maybe Val
deref = S.lookup

----- CORE TYPES
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
  | App Exp Exp
  | Diff Exp Exp
  | Op Ident [ Exp ]
  | SetExp Ident Exp
  | BeginExp [Exp]
  deriving (Show, Eq)

data Proc = Proc [ Ident ] Exp Env
  deriving (Show, Eq)

data Val
  = NumVal Int
  | BoolVal Bool
  | ProcVal Proc
  deriving (Show,Eq)

----- CONVERSIONS
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

instance ToVal Proc where
  toVal = ProcVal

instance ToVal Val where
  toVal = id

-- ----- TRACING
-- showTrace :: Exp -> String
-- showTrace c@(Const _) = show c
-- showTrace (If {}) = "if"
-- showTrace (Let defs _) = let ds = unwords $ fmap fst defs
--                          in "let " <> ds
-- showTrace (Letrec defs _) = let ds = unwords $ fmap (\(x,_,_) -> x) defs
--                             in "letrec " <> ds
-- showTrace (ProcExp vars _) = "proc " <> unwords vars <> " = ..."
-- showTrace (App (Var name) _) = "app " <> name
-- showTrace (App _ _) = "app anon"
-- showTrace (Diff _ _) = "diff"
-- showTrace (Op name _) = "op " <> name
-- showTrace (Var name) = "var " <> name
