{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ch03.ProcLang.Operators1 where

import Ch03.ProcLang.Types

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
