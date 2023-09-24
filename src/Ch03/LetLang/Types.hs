-- |

module Ch03.LetLang.Types where

newtype Program = Program Exp

type Ident = String

data Exp
  = Const Int
  | Diff Exp Exp
  | Zero Exp
  | If Exp Exp Exp
  | Var Ident
  | Let Ident Exp Exp
  deriving (Show)

data Val
  = NumVal Int
  | BoolVal Bool
  deriving (Show)
