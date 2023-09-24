{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}

module Ch01 where

import Text.Printf (printf)
import Ch01.LcExp

inS :: Int -> Bool

inS n
  | n == 0 = True
  | n - 3 >= 0 = inS $ n - 3
  | otherwise = False

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

nthElement :: [a] -> Int -> a
nthElement [] _ = error "empty list"
nthElement (x : _) 0 = x
nthElement (x : xs) n = nthElement xs (n - 1)

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y : ys)
  | x == y = ys
  | otherwise = y : removeFirst x ys

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys)
  | x == y = remove x ys
  | otherwise = y : remove x ys

occursFree :: String -> LcExp -> Bool
occursFree sym (Identifier  s) = sym == s
occursFree sym (Lambda x body) = sym /= x && occursFree sym body
occursFree sym (App rator rand) = occursFree sym rator || occursFree sym rand

data SList
  = Nil
  | Cons Sexp SList

data Sexp
  = Symbol String
  | List SList

subst :: String -> String -> SList -> SList
subst _ _ Nil = Nil
subst old new (Cons (Symbol s) cdr) =
  Cons r (subst old new cdr)
  where
    r = if s == old then Symbol new else Symbol s
subst old new (Cons (List carSL) cdr) =
  Cons (List $ subst old new carSL) (subst old new cdr)

numberElementsFrom :: Int -> [a] -> [(Int, a)]
numberElementsFrom _ [] = []
numberElementsFrom i (x : xs) = (i, x) : numberElementsFrom (i + 1) xs

numberElements :: [a] -> [(Int, a)]
numberElements = numberElementsFrom 0

listSum :: [Int] -> Int
listSum [] = 0
listSum (n : ns) = n + listSum ns

duple :: Int -> a -> [a]
duple 0 x = []
duple n x = x : duple (n - 1) x

down :: [a] -> [[a]]
down = map (: [])

swapper :: String -> String -> SList -> SList
swapper _ _ Nil = Nil
swapper x y (Cons (Symbol s) sl) =
  Cons (Symbol r) (swapper x y sl)
  where
    r
      | s == x = y
      | s == y = x
      | otherwise = s
swapper x y (Cons (List sl') sl) =
  Cons (List (s sl')) (s sl)
  where
    s = swapper x y

listSet :: [a] -> Int -> a -> [a]
listSet [] _ _ = error "index out of bounds"
listSet (_ : xs) 0 y = y : xs
listSet (x : xs) i y = x : listSet xs (i - 1) y

countOccurrences :: String -> SList -> Int
countOccurrences s Nil = 0
countOccurrences s (Cons (Symbol sym) sl) =
  (if s == sym then 1 else 0) + countOccurrences s sl
countOccurrences s (Cons (List sl') sl) =
  countOccurrences s sl' + countOccurrences s sl

product' :: [String] -> [String] -> [(String, String)]
product' [] _ = []
product' (x : xs) ys = map (x,) ys ++ product' xs ys

filterIn :: (a -> Bool) -> [a] -> [a]
filterIn _ [] = []
filterIn pred (x : xs) =
  if pred x
    then x : filterIn pred xs
    else filterIn pred xs

listIndex :: (a -> Bool) -> [a] -> Maybe Int
listIndex pred = li 0
  where
    li _ [] = Nothing
    li i (x : xs)
      | pred x = Just i
      | otherwise = li (i + 1) xs

every :: (a -> Bool) -> [a] -> Bool
every _ [] = True
every pred (x : xs)
  | pred x = every pred xs
  | otherwise = False

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists pred (x : xs)
  | pred x = True
  | otherwise = exists pred xs

up :: [[a]] -> [a]
up [] = []
up (xs : xss) = xs ++ up xss

merge :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
merge _ [] [] = []
merge _ [] ys = ys
merge _ xs [] = xs
merge pred (x : xs) (y : ys)
  | pred x y = y : merge pred (x : xs) ys
  | otherwise = x : merge pred xs (y : ys)

sortPred :: (Int -> Int -> Bool) -> [Int] -> [Int]
sortPred _ [] = []
sortPred _ [x] = [x]
sortPred pred xs = merge pred (sort firstHalf) (sortPred pred secondHalf)
  where
    len = length xs
    d = len `div` 2
    firstHalf = take d xs
    secondHalf = drop d xs

sort = sortPred (<)

data BinTree = Lv Int | B String BinTree BinTree
  deriving Show

doubleTree :: BinTree -> BinTree
doubleTree (Lv i) = Lv (2 * i)
doubleTree (B s l r) = B s (doubleTree l) (doubleTree r)

aTree = B "red" (B "bar" (Lv 26) (Lv 12))
                (B "red" (Lv 11)
                         (B "quux" (Lv 117) (Lv 14)))


markLeavesWithRedDepth' :: Int -> BinTree -> BinTree
markLeavesWithRedDepth' n (Lv i) = Lv n
markLeavesWithRedDepth' n (B "red" l r) =
  B "red" (markLeavesWithRedDepth' (n + 1) l) (markLeavesWithRedDepth' (n + 1) r)
markLeavesWithRedDepth' n (B s l r) =
  B s (markLeavesWithRedDepth' n l) (markLeavesWithRedDepth' n r)

markLeavesWithRedDepth :: BinTree -> BinTree
markLeavesWithRedDepth = markLeavesWithRedDepth' 0

data BST = Null | Bst Int BST BST

aBST = Bst 14 (Bst 7 Null (Bst 12 Null Null))
              (Bst 26 (Bst 20 (Bst 17 Null Null)
                              Null)
                      (Bst 31 Null Null))
data Direction = L | R
  deriving Show

path :: Int -> BST -> [Direction]
path n = reverse . recur []
  where
    recur acc Null = acc
    recur acc (Bst i l r)
      | i == n = acc
      | i > n = recur (L : acc) l
      | otherwise = recur (R : acc) r


anotherTree = B "foo" (B "bar" (Lv 26) (Lv 12))
                (B "baz" (Lv 11)
                         (B "quux" (Lv 117) (Lv 14)))


numberLeaves :: BinTree -> BinTree
numberLeaves = fst . recur 0
  where
    recur i (Lv _) = (Lv i, i + 1)
    recur i (B s l r) =
      let (l', i') = recur i l
          (r', i'') = recur i' r
      in (B s l' r', i'')


numberElements' :: [a] -> [(Int, a)]
numberElements' [] = []
numberElements' (x:xs) = g (0,x) $ numberElements' xs
  where
    g last [] = [last]
    g first ((i,y):ys) = first : g (i + 1, y) ys
