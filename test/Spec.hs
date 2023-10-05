-- |

-- module Main where

import ProcLangSpec as ProcLang
import LetrecLangSpec as LetrecLang
import LexAddrLangSpec as LexAddrLang
import ExplicitRefsLangSpec as ExplicitRefsLang

import Test.Hspec

main :: IO ()
main = hspec $ do
  ProcLang.spec
  LetrecLang.spec
  LexAddrLang.spec
  ExplicitRefsLang.spec
