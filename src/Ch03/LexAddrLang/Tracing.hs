-- |

module Ch03.LexAddrLang.Tracing where

import Ch03.LexAddrLang.Types

showTrace :: Exp -> String
showTrace c@(Const _) = show c
showTrace (If {}) = "if"
showTrace (Let defs _) = let ds = unwords $ fmap fst defs
                         in "let " <> ds
-- showTrace (Letrec defs _) = let ds = unwords $ fmap (\(x,_,_) -> x) defs
--                             in "letrec " <> ds
showTrace (ProcExp vars _) = "proc " <> unwords vars <> " = ..."
showTrace (App (Var name) _) = "app " <> name
showTrace (App _ _) = "app anon"
showTrace (Diff _ _) = "diff"
showTrace (Op name _) = "op " <> name
showTrace (Var name) = "var " <> name
showTrace (NVar i) = "nvar " <> show i
showTrace (NLet defs _) = "nlet " <> show (length defs)
