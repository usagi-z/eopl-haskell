{-# LANGUAGE FlexibleInstances #-}

module Ch03.LetLangExtended.Types where

newtype Program = Program Exp
  deriving (Show)

type Ident = String

data Exp
  = Const Int
  | If Exp Exp Exp
  | Cond [(Exp, Exp)]
  | Var Ident
  | Let [ (Ident, Exp) ] Exp
  | Unpack [ Ident ] Exp Exp
  | Diff Exp Exp
  | Op0 Ident
  | Op1 Ident Exp
  | Op2 Ident Exp Exp
  | OpN Ident [ Exp ]
  deriving (Show)

data Val
  = NumVal Int
  | BoolVal Bool
  | ListVal [Val]
  deriving (Show)

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
