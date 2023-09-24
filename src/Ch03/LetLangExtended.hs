{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ch03.LetLangExtended where

import Data.Kind (Type)
import Ch03.LetLangExtended.Types
import Ch03.LetLangExtended.Parsing
import Data.Function (on)
import Data.List (find)
import Data.Foldable (foldrM, foldlM)



-- test programs
testLet = "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8), y)"
testZero = "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in zero?(-(-(x,8), y))"
testMinus = "let x = 7 in let y = 2 in let y = let x = minus(x) in -(x,y) in zero?(-(-(x,8), minus(y)))"
testList = "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))"
testCond = "cond zero?(9) ==> 9 zero?(0) ==> 0 zero?(1) ==> 1 end"
testCondFail = "cond zero?(9) ==> 9 zero?(3) ==> 0 zero?(1) ==> 1 end"
testLetMulti = "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)"
testUnpack = "let u = 7 in unpack x y = cons(u,cons(3,emptylist)) in -(x,y)"


-- Env
class Environment (e :: Type -> Type) where
  type Var e :: Type
  emptyEnv :: e v
  applyEnv :: e v -> Var e -> v
  extendEnv :: Var e -> v -> e v -> e v
  extendEnvStar :: [Var e] -> [v] -> e v -> e v
  extendEnvStar vars vals env = foldr (uncurry extendEnv) env $ zip vars vals

newtype Lst var val = L [(var,val)]
  deriving (Show)

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




-- Operators
--- helpers
unaryOp :: forall a. FromVal a => (a -> Val) -> String -> Val -> Val
unaryOp f msg = f . fromVal @a msg

binaryOp :: forall a b. (FromVal a, FromVal b)
         => (a -> b -> Val) -> String -> Val -> Val -> Val
binaryOp f msg x y =
  let
    x' = fromVal @a msg x
    y' = fromVal @b msg y
  in f x' y'

nAryOp :: forall a. FromVal a => ([a] -> Val) -> String -> [Val] -> Val
nAryOp f msg xs = f $ fmap (fromVal @a msg) xs

asNum f y x = NumVal $ f x y
asBool f y x = BoolVal $ f x y
asList f y x = ListVal $ f x y


--- definitions
emptylist = ListVal []

zero :: Val -> Val
zero = unaryOp @Int (BoolVal . (== 0)) "zero?: "

minus :: Val -> Val
minus = unaryOp @Int (NumVal . negate) "minus: "

add = binaryOp (asNum (+)) "add: "
sub = binaryOp (asNum (-)) "sub: "
mul = binaryOp (asNum (*)) "mul: "
quot' = binaryOp (asNum quot) "quot: "

equal = binaryOp @Int @Int (asBool (==)) "equal: "
greater = binaryOp @Int @Int (asBool (>)) "greater: "
less = binaryOp @Int @Int (asBool (<)) "less: "

cons x xs = let xs' = fromVal @[Val] "cons: " xs
            in ListVal $ x : xs'

car = unaryOp @[Val] f "car: "
  where f (x:_) = x

cdr = unaryOp @[Val] f "cdr: "
  where f (_:xs) = ListVal xs

null' = unaryOp @[Val] f "null?: "
  where f [] = BoolVal True
        f _ = BoolVal False

list = ListVal

--- lookup tables
operators0 = [ ("emptylist", emptylist) ]

operators1 = [ ("zero?", zero)
             , ("minus", minus)
             , ("car", car)
             , ("cdr", cdr)
             , ("null?", null')
             ]

operators2 :: [(String, Val -> Val -> Val)]
operators2 = [ ("add", add)
             , ("sub", sub)
             , ("mul", mul)
             , ("quot", quot')
             , ("cons", cons)
             ]

operatorsN = [ ("list", list) ]



-- evaluation
valueOf :: Exp -> Env -> Val
valueOf exp env =
  case exp of
    Const n -> NumVal n
    Var ident -> case lookup ident operators0 of
                   Nothing -> applyEnv env ident
                   Just o -> o
    Diff m s -> case (valueOf m env , valueOf s env) of
                  (NumVal m', NumVal s') -> NumVal $ m' - s'
                  (m',s') -> error $ "Diff type error: minuend - " <> show m' <>
                                     ", subtrahend - " <> show s'
    Op1 name x -> case lookup name operators1 of
                    Nothing -> error $ "operator " <> name <> " not defined"
                    Just f -> f $ valueOf x env
    Op2 name x y -> case lookup name operators2 of
                      Nothing -> error $ "operator " <> name <> " not defined"
                      Just f -> on f (`valueOf` env) x y
    OpN name xs -> case lookup name operatorsN of
                      Nothing -> error $ "operator " <> name <> " not defined"
                      Just f -> f $ fmap (`valueOf` env) xs
    If c t e -> let val = valueOf c env
                    cb = fromVal @Bool "If condition: " val
                in if cb
                   then valueOf t env
                   else valueOf e env
    Cond conds ->
      let f (c, _) = fromVal @Bool "Cond" $ valueOf c env
      in case find f conds of
           Nothing -> error "No branch taken in a cond"
           Just (_,body) -> valueOf body env
    Unpack ids listExp body ->
      let l = fromVal @[Val] "unpack" $ valueOf listExp env
          env' = extendEnvStar ids l env
      in valueOf body env'
    -- let* in the book
    Let defs b -> let f env (ident, defExp) =
                        extendEnv ident (valueOf defExp env) env
                      env' = foldl f env defs
                  in valueOf b env'

valueOfProgram :: Program -> Val
valueOfProgram (Program e) = valueOf e initEnv

run :: String -> Either String Val
run = fmap valueOfProgram . scanAndParse
