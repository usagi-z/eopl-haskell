{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
module Ch03.LetLang where

import Data.Kind (Type)
import Ch03.LetLang.Types
import Ch03.LetLang.Parsing

testLet = "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8), y)"

class Environment (e :: Type -> Type) where
  type Var e :: Type
  emptyEnv :: e v
  applyEnv :: e v -> Var e -> v
  extendEnv :: Var e -> v -> e v -> e v
  extendEnvStar :: [Var e] -> [v] -> e v -> e v
  extendEnvStar vars vals env = foldr (uncurry extendEnv) env $ zip vars vals

newtype Lst var val = L [(var,val)]

instance Environment (Lst String) where
  type Var (Lst String) = String
  emptyEnv = L []
  applyEnv (L []) _ = error "empty env"
  applyEnv (L (x:xs)) var | let (var',val) = x, var == var' = val
                          | otherwise = applyEnv (L xs) var
  extendEnv var val (L env) = L $ (var,val) : env

type Env = Lst String Val

initEnv :: Env
initEnv = extendEnvStar
            ["i", "v", "x"]
            (map NumVal [1,5,10])
            emptyEnv

valueOfProgram :: Program -> Val
valueOfProgram (Program e) = valueOf e initEnv

run :: String -> Either String Val
run s = valueOfProgram <$> scanAndParse s

-- boolFromVal :: String -> Val -> Bool
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

valueOf :: Exp -> Env -> Val
valueOf exp env =
  case exp of
    Const n -> NumVal n
    Var ident -> applyEnv env ident
    Diff m s -> case (valueOf m env , valueOf s env) of
                  (NumVal m', NumVal s') -> NumVal $ m' - s'
                  (m',s') -> error $ "Diff type error: minuend - " <> show m' <>
                                     ", subtrahend - " <> show s'
    Zero e -> let val = valueOf e env
                  i = fromVal @Int "Zero: " val
              in BoolVal $ i == 0
    If c t e -> let val = valueOf c env
                    cb = fromVal @Bool "If condition: " val
                in if cb
                   then valueOf t env
                   else valueOf e env
    Let x d b -> let def = valueOf d env
                     env' = extendEnv x def env
                 in valueOf b env'
