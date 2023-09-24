{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}


module Ch03.ProcLang.Operators2 where

import Ch03.ProcLang.Types
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Data.Proxy (Proxy(..))

data ArgList (ts :: [Type]) where
  ANil :: ArgList '[]
  ACons :: t -> ArgList ts -> ArgList (t ': ts)

class ToArgList ts where
  toArgList :: [Val] -> ArgList ts

instance ToArgList '[] where
  toArgList [] = ANil

instance (FromVal t, ToArgList ts) => ToArgList (t ': ts) where
  toArgList (v:vs) = ACons (fromVal "" v) (toArgList @ts vs)

-- op :: forall ts. AllFromVal ts
--    => FunOfArgs ts -> [ Val ] -> Val

class FunOfArgs (ts :: [Type]) where
  type F ts :: Type
  appF :: Proxy ts -> F ts -> ArgList ts -> Val

instance FunOfArgs '[] where
  type F '[] = Val
  appF p f ANil = f

instance FunOfArgs ts => FunOfArgs (t ': ts) where
  type F (t ': ts) = t -> F ts
  appF _ f (ACons x xs ) = appF (Proxy @ts) (f x) xs

op :: (FunOfArgs ts, ToArgList ts) => Proxy ts -> F ts -> [Val] -> Val
op p f vs = appF p f ( toArgList vs )

add'' x y = NumVal $ x + y
add' = op (Proxy @'[Int, Int]) add''


-- Operators
--- helpers
-- add' :: ArgList '[Int, Int] -> Val
-- add' (ACons x (ACons y ANil))= NumVal $ x + y
-- op'' :: ToArgList ts => (ArgList ts -> Val) -> [Val] -> Val
-- op'' f = f . toArgList
