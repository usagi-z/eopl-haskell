-- |

module ProcLangSpec where

import Test.Hspec

import Ch03.ProcLang
import Ch03.ProcLang.Types (Val(..))
import Ch03.ProcLang.Tests
import Data.Functor ((<&>))

spec :: Spec
spec = do
  let runFromFile f = readFile f <&> run
  describe "run ProcLang" $ do
    it "let" $ do
      run "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8), y)"
        `shouldBe` Right (NumVal (-5))
    it "zero" $ do
      run "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in zero?(-(-(x,8), y))"
        `shouldBe` Right (BoolVal False)
    it "minus" $ do
      run  "let x = 7 in let y = 2 in let y = let x = minus(x) in -(x,y) in zero?(-(-(x,8), minus(y)))"
        `shouldBe` Right (BoolVal False)
    it "list" $ do
      run  "let x = 4 in cons(x, cons(cons(-(x,1), emptylist()), emptylist()))"
        `shouldBe` Right (ListVal [NumVal 4,ListVal [NumVal 3]])
    it "letMulti" $ do
      run "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)"
        `shouldBe` Right (NumVal 2)
    it "proc1" $ do
      run "let f = proc (x) -(x,11) in (f (f 77))"
        `shouldBe` Right (NumVal 55)
    it "proc2" $ do
      run "(proc (f) (f (f 77)) proc (x) -(x,11))"
        `shouldBe` Right (NumVal 55)
    it "proc3" $ do
      run "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))"
        `shouldBe` Right (NumVal (-100))
    it "procMulti" $ do
      run "let f = proc (x,y) -(x,y) in (f 77 53)"
        `shouldBe` Right (NumVal 24)
    it "y" $ do
      runFromFile "test/y.proclang" `shouldReturn` Right (NumVal 12)
    it "odd" $ do
      runFromFile "test/oddEven.proclang" `shouldReturn` Right (BoolVal True)
    it "dyn" $ do
      runFromFile "test/dyn.proclang" `shouldReturn` Right (NumVal 8)
