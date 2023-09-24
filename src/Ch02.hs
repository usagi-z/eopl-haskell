{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Ch02 where


import Text.Printf (printf)
import Data.Kind (Type)
import Control.Exception (catch)
import Data.Maybe (isJust)
import Ch01.LcExp

class NatNum a where
  zero :: a
  isZero :: a -> Bool
  successor :: a -> a
  predecessor :: a -> a


plus :: NatNum a => a -> a -> a
plus n m | isZero n = m
         | otherwise = plus (predecessor n) (successor m)

data T = T

instance NatNum [T] where
  zero = []
  isZero = null
  successor = (T :)
  predecessor = tail


instance NatNum Int where
  zero = 0
  isZero = (== 0)
  successor = (+ 1)
  predecessor x = x - 1


type Bignum16 = [Int]

consLSB :: Int -> Bignum16 -> Bignum16
consLSB n xs | n >= 0 && n < 16 = n : xs
             | otherwise = error "added bigit out of range"

fromInt :: Int -> Bignum16
fromInt 0 = []
fromInt n = consLSB r (fromInt q)
  where (q,r) = divMod n 16

toInt :: Bignum16 -> Int
toInt [] = 0
toInt (n:ns) = n + 16 * toInt ns

instance NatNum Bignum16 where
  zero = []
  isZero = null
  successor [] = [1]
  successor (n:ns) | n == 15 = consLSB 0 (successor ns)
                   | otherwise = (n + 1) : ns
  predecessor [] = error "predecessor of zero"
  predecessor [1] = []
  predecessor (n:ns) | n == 0 = predecessor ns
                     | otherwise = (n - 1) : ns



data DiffT = One
           | Diff DiffT DiffT
  deriving Show


diffToInt :: DiffT -> Int
diffToInt One = 1
diffToInt (Diff p n) = diffToInt p - diffToInt n

zeroD = Diff One One
oneD = Diff One zeroD
minusOneD = Diff zeroD One
twoD = Diff oneD minusOneD

instance NatNum DiffT where
  zero = zeroD
  isZero = (== 0) . diffToInt
  successor One = twoD
  successor (Diff p n) = Diff (successor p) n
  predecessor One = zeroD
  predecessor (Diff p n) = Diff (predecessor p) n

diffPlus :: DiffT -> DiffT -> DiffT
diffPlus x (Diff yp yn) = Diff x (Diff yn yp)


class Environment (e :: Type -> Type) where
  type Var e :: Type
  emptyEnv :: e v
  applyEnv :: e v -> Var e -> v
  extendEnv :: Var e -> v -> e v -> e v
  extendEnvStar :: [Var e] -> [v] -> e v -> e v
  extendEnvStar vars vals env = foldr (uncurry extendEnv) env $ zip vars vals

newtype Lst var val = L [(var,val)]

instance Environment (Lst String) where
  type Var (Lst String) = String
  emptyEnv = L []
  applyEnv (L []) _ = error "empty env"
  applyEnv (L (x:xs)) var | let (var',val) = x, var == var' = val
                          | otherwise = applyEnv (L xs) var
  extendEnv var val (L env) = L $ (var,val) : env


data Env var val = EmptyEnv
                 | ExtendEnv var val (Env var val)


instance Environment (Env String) where
  type Var (Env String) = String
  emptyEnv = EmptyEnv
  applyEnv EmptyEnv _ = error "empty env"
  applyEnv (ExtendEnv var val env) var' | var == var' = val
                                        | otherwise = applyEnv env var'

  extendEnv = ExtendEnv


type ReaderS = ((->) String)

instance Environment ReaderS where
  type Var ((->) String) = String
  emptyEnv _ = error "empty env"
  applyEnv = ($)
  extendEnv var val env sVar =
    if var == sVar
    then val
    else env sVar



-- ex 2.12
-- data StackOp = Push | Pop

emptyStack _ = error "stack empty"
pushStack el s _ = (el,s)
popStack s = s ()

-- ex 2.13

-- emptyEnvP :: ReaderS v -> IO Bool
-- emptyEnvP env = try $
emptyEnv' = (error "env empty", True)
applyEnv' (f,e) s = if e then Nothing else f s
extendEnv' var val env = (\s -> if var == s
                                then Just val
                                else applyEnv' env s
                         , False
                         )
isEmpty' = snd

-- ex 2.14
hasBinding' s env = isJust $ applyEnv' env s


-- 2.16

data NodeInSeq = NodeInSeq Int [Int] [Int]
  deriving Show
currentElement :: NodeInSeq -> Int
currentElement (NodeInSeq c _ _) = c

moveLeft :: NodeInSeq -> NodeInSeq
moveLeft n@(NodeInSeq _ [] _) = n
moveLeft (NodeInSeq c (l:ls) rs) = NodeInSeq l ls (c:rs)

moveRight :: NodeInSeq -> NodeInSeq
moveRight n@(NodeInSeq _ _ []) = n
moveRight (NodeInSeq c ls (r:rs)) = NodeInSeq r (c:ls) rs

insertLeft :: Int -> NodeInSeq -> NodeInSeq
insertLeft i (NodeInSeq c ls rs) = NodeInSeq c (i:ls) rs

insertRight :: Int -> NodeInSeq -> NodeInSeq
insertRight i (NodeInSeq c ls rs) = NodeInSeq c ls (i:rs)



-- 2.19

data BinTree = EBT | BT Int BinTree BinTree
 deriving Show

numberToBT :: Int -> BinTree
numberToBT i = BT i EBT EBT

insertToLeft :: Int -> BinTree -> BinTree
insertToLeft i EBT = numberToBT i
insertToLeft i (BT j EBT r) = BT j (numberToBT i) r
insertToLeft i (BT j (BT k ll lr) r) = BT j (BT i (insertToLeft k ll) lr) r

insertToRight :: Int -> BinTree -> BinTree
insertToRight i EBT = numberToBT i
insertToRight i (BT j l EBT) = BT j l (numberToBT i)
insertToRight i (BT j l (BT k ll lr)) = BT j l (BT i ll (insertToRight k lr))

atLeaf :: BinTree -> Bool
atLeaf EBT = True
atLeaf _ = False

data Z = Z [BinTree -> BinTree] BinTree

instance Show Z where
  show (Z _ t) = show t

currentBinTree :: Z -> BinTree
currentBinTree (Z _ t) = t


moveToLeft' :: Z -> Z
moveToLeft' z@(Z us EBT) = z
moveToLeft' (Z us (BT i l r)) = Z ((\l -> BT i l r) : us) l

moveToRight' :: Z -> Z
moveToRight' z@(Z us EBT) = z
moveToRight' (Z us (BT i l r)) = Z ((\r -> BT i l r) : us) r

moveUp' :: Z -> Z
moveUp' (Z (u:us) t) = Z us (u t)

atRoot' :: Z -> Bool
atRoot' (Z [] _) = True
atRoot' _ = False

toRoot :: Z -> BinTree
toRoot (Z [] t) = t
toRoot (Z (u:us) t) = toRoot $ Z us (u t)

-- 2.21
hasBinding'' :: String -> Env String v -> Bool
hasBinding'' s EmptyEnv = False
hasBinding'' s (ExtendEnv var val env) = (s == var) || hasBinding'' s env


-- 
