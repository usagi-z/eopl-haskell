-- |

module Ch03.LexAddrLang.Environment where

import Ch03.LexAddrLang.Types
import Data.List.Extra ((!?), elemIndex)
import Ch03.LetrecLang.Types (extendEnv)


applySEnv :: Ident -> SEnv -> Int
applySEnv var senv = case elemIndex var senv of
                       Nothing -> error $ "var not bound " <> var
                       Just n -> n
emptySEnv = []
extendSEnv = (:)

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
