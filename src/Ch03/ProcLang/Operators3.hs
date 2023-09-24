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


module Ch03.ProcLang.Operators3 where

import Ch03.ProcLang.Types
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import Data.Proxy (Proxy(..))



-- type family Args a
class FunOfArgs (as :: [Type]) where
  type F as :: Type -- | F ts -> rt
  -- appF ::  Proxy as -> F as -> ArgList as -> Val

instance FunOfArgs '[] where
  type F '[] = TypeError (Text "uh-oh")

data ArgList (ts :: [Type]) where
  ANil :: ArgList '[]
  ACons :: t -> ArgList ts -> ArgList (t ': ts)

class ToArgList ts where
  toArgList :: [Val] -> ArgList ts

instance ToArgList '[] where
  toArgList [] = ANil

instance (FromVal t, ToArgList ts) => ToArgList (t ': ts) where
  toArgList (v:vs) = ACons (fromVal "" v) (toArgList @ts vs)

-- op :: (ts ~ Args fs, rt ~ Ret fs, FunOfArgs fs, ToArgList ts, ToVal rt)
--    => Proxy fs -> F fs -> [Val] -> Val
-- op p f vs = toVal $ appF p f ( toArgList vs )

-- add = op (Proxy @('FunSig ))
