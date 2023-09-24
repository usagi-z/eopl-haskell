{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Ch03.LexAddrLang.Types where

import Data.Kind (Type)
import Data.Foldable (find)
import Data.List (findIndex, elemIndex)

type Env = [Val]
type SEnv = [IdentWithBindingInfo]

newtype Program = Program Exp
  deriving (Show)

type Ident = String

data IdentWithBindingInfo
  = NonRec Ident
  | Rec Ident

getIdentName :: IdentWithBindingInfo -> Ident
getIdentName (NonRec idnt) = idnt
getIdentName (Rec idnt) = idnt

data Exp
  = Const Int
  | Var Ident
  | If Exp Exp Exp
  -- | Let [ (Ident, Exp) ] Exp
  | Let Ident Exp Exp
  -- | Letrec [ (Ident, [ Ident ], Exp) ] Exp
  | Letrec Ident Ident Exp Exp
  | ProcExp [ Ident ] Exp
  | App Exp [ Exp ]
  | Diff Exp Exp
  | Op Ident [ Exp ]
  | NVar Int
  | NLetrecVar Int
  | NLet Exp Exp
  | NLetrec Exp Exp
  | NProc Exp
  deriving (Show, Eq)

data Proc = Proc Exp Env
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
