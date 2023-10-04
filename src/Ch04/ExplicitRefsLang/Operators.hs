{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}


module Ch04.ExplicitRefsLang.Operators where

import Ch04.ExplicitRefsLang.Types
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Data.Proxy (Proxy(..))
-- import Ch04.ExplicitRefsLang.Operators1 (operators0)



data FunSig = FunSig [Type] Type
type family Args (fs :: FunSig) :: [Type]
type instance Args ('FunSig as rt) = as
type family Ret (fs :: FunSig) :: Type
type instance Ret ('FunSig as rt) = rt

class FunOfArgs (fs :: FunSig) where
  type F fs :: Type
  appF :: (as ~ Args fs) => Proxy fs -> F fs -> ArgList as -> Ret fs

instance FunOfArgs ('FunSig '[] rt) where
  type F ('FunSig '[] rt) = rt
  appF p f ANil = f

instance FunOfArgs ('FunSig as rt) => FunOfArgs ('FunSig (a ': as) rt) where
  type F ('FunSig (a ': as) rt) = a -> F ('FunSig as rt)
  appF :: Proxy ('FunSig (a ': as) rt)
       -> F ('FunSig (a ': as) rt)
       -> ArgList (a ': as)
       -> Ret ('FunSig (a ': as) rt)
  appF _ f (ACons x xs ) = appF (Proxy @('FunSig as rt)) (f x) xs


data ArgList (ts :: [Type]) where
  ANil :: ArgList '[]
  ACons :: t -> ArgList ts -> ArgList (t ': ts)

class ToArgList ts where
  toArgList :: String -> [Val] -> ArgList ts

instance ToArgList '[] where
  toArgList _ [] = ANil
  toArgList msg (_:_) = error $ msg <> ": argument number mismatch"

instance (FromVal t, ToArgList ts) => ToArgList (t ': ts) where
  toArgList msg [] = error $ msg <> ": argument number mismatch"
  toArgList msg (v:vs) = ACons (fromVal msg v) (toArgList @ts msg vs)

op :: ( ts ~ Args fs
      , rt ~ Ret fs
      , FunOfArgs fs
      , ToArgList ts
      , ToVal rt)
   => Proxy fs
   -> String
   -> F fs
   -> [Val] -> Val
op p msg f vs = toVal $ appF p f ( toArgList msg vs )




-- operation definitions
minus = op (Proxy @('FunSig '[Int] Int)) "minus" negate
add = op (Proxy @('FunSig '[Int, Int] Int)) "add" (+)
sub = op (Proxy @('FunSig '[Int, Int] Int)) "sub" (-)
mul = op (Proxy @('FunSig '[Int, Int] Int)) "mul" (*)
quot' = op (Proxy @('FunSig '[Int, Int] Int)) "quot" quot

zero = op (Proxy @('FunSig '[Int] Bool)) "zero" (== 0)
equal =  op (Proxy @('FunSig '[Int, Int] Bool)) "equal" (==)
less =  op (Proxy @('FunSig '[Int, Int] Bool)) "less" (<)
greater =  op (Proxy @('FunSig '[Int, Int] Bool)) "greater" (>)

cons = op (Proxy @('FunSig '[Val, [Val]] [Val])) "cons" (:)
car = op (Proxy @('FunSig '[[Val]] Val)) "car" head
cdr = op (Proxy @('FunSig '[[Val]] [Val])) "cdr" tail
null' = op (Proxy @('FunSig '[[Val]] Bool)) "null" null
list = op (Proxy @('FunSig '[[Val]] [Val])) "list" id
emptylist = op (Proxy @('FunSig '[] [Val])) "list" mempty

operators = [ ("zero?", zero)
            , ("greater", greater)
            , ("less", less)
            , ("minus", minus)
            , ("emptylist", emptylist)
            , ("cons", cons)
            , ("null?", null')
            , ("car", car)
            , ("cdr", cdr)
            , ("list", list)
            , ("add", add)
            , ("sub", sub)
            , ("mul", mul)
            , ("quot", quot')
            ]
