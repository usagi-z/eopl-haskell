-- |

module Ch03.LetrecLang.Tests where

import Ch03.LetrecLang
-- test programs
-- -5
testLet = "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8), y)"
-- False
testZero = "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in zero?(-(-(x,8), y))"
-- False
testMinus = "let x = 7 in let y = 2 in let y = let x = minus(x) in -(x,y) in zero?(-(-(x,8), minus(y)))"
-- Right (ListVal [NumVal 4,ListVal [NumVal 3]])
testList = "let x = 4 in cons(x, cons(cons(-(x,1), emptylist()), emptylist()))"
-- Right (NumVal 2)
testLetMulti = "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)"
-- p. 75
-- Right (NumVal 55)
testProc1 = "let f = proc (x) -(x,11) in (f (f 77))"
testProc2 = "(proc (f) (f (f 77)) proc (x) -(x,11))"
-- p. 76
-- Right (NumVal (-100))
testProc3 = "let x = 200 in let f = proc (z) -(z,x) in let x = 100 in let g = proc (z) -(z,x) in -((f 1), (g 1))"
-- testLetProc1 = "letproc f = (x) -(x,11) in (f (f 77))"
-- Right (NumVal 24)
testProcMulti = "let f = proc (x,y) -(x,y) in (f 77 53)"
