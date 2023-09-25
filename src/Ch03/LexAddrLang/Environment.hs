-- |

module Ch03.LexAddrLang.Environment where

import Ch03.LexAddrLang.Types
import Data.List.Extra ((!?), elemIndex, findIndex)
import Ch03.LetrecLang.Types (extendEnv)


applySEnv' :: Int -> Ident -> SEnv -> (Int, Bool)
applySEnv' _ var [] = error $ "var " <> var <> " not defined"
applySEnv' i var (idnt : rest) =
  let (var', rec) =
        case idnt of
          (NonRec var') -> (var', False)
          (Rec var') -> (var', True)
  in if var == var'
     then (i, rec)
     else applySEnv' (i + 1) var rest

applySEnv = applySEnv' 0

emptySEnv = []

extendSEnv :: IdentWithBindingInfo -> SEnv -> SEnv
extendSEnv = (:)

extendSEnvStar :: [ IdentWithBindingInfo ] -> SEnv -> SEnv
extendSEnvStar = (<>)

applyEnv :: Env -> Int -> Val
applyEnv env i = case env !? i of
                   Nothing -> error "env index out of bounds"
                   Just val -> val
emptyEnv :: Env
emptyEnv = []
extendEnv :: Val -> Env -> Env
extendEnv = (:)
extendEnvStar :: [Val] -> Env -> Env
extendEnvStar = (<>)
