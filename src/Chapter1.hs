{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE TupleSections #-}
module Chapter1 where


inS :: Int -> Bool
inS n | n == 0 = True
      | n - 3 >= 0 = inS $ n - 3
      | otherwise = False


listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs


nthElement :: [a] -> Int -> a
nthElement [] _ = error "empty list"
nthElement (x:_) 0 = x
nthElement (x:xs) n = nthElement xs (n - 1)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) | x == y = ys
                     | otherwise = y : removeFirst x ys

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys) | x == y = remove x ys
                | otherwise = y : remove x ys




data LcExp
  = Identifier String
  | Lambda String LcExp
  | App LcExp LcExp

occursFree :: String -> LcExp -> Bool
occursFree sym (Identifier s) = sym == s
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
  Cons r (subst old new cdr) where
    r = if s == old then Symbol new else Symbol s
subst old new (Cons (List carSL) cdr) =
  Cons (List $ subst old new carSL) (subst old new cdr)


numberElementsFrom :: Int -> [a] -> [(Int,a)]
numberElementsFrom _ [] = []
numberElementsFrom i (x : xs) = (i,x) : numberElementsFrom (i + 1) xs

numberElements :: [a] -> [(Int, a)]
numberElements = numberElementsFrom 0


listSum :: [Int] -> Int
listSum [] = 0
listSum (n : ns) = n + listSum ns



duple :: Int -> a -> [a]
duple 0 x = []
duple n x = x : duple (n - 1) x

down :: [a] -> [[a]]
down = map (:[])

swapper :: String -> String -> SList -> SList
swapper _ _ Nil = Nil
swapper x y (Cons (Symbol s) sl) =
  Cons (Symbol r) (swapper x y sl) where
    r | s == x = y
      | s == y = x
      | otherwise = s
swapper x y (Cons (List sl') sl) =
  Cons (List (s sl')) (s sl) where
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


product' :: [String] -> [String] -> [(String,String)]
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
  where li _ [] = Nothing
        li i (x : xs) | pred x = Just i
                      | otherwise = li (i + 1) xs

every :: (a -> Bool) -> [a] -> Bool
every _ [] = True
every pred (x : xs) | pred x = every pred xs
                    | otherwise = False

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists pred (x : xs) | pred x = True
                     | otherwise = exists pred xs

up :: [[a]] -> [a]
up [] = []
up (xs : xss) = xs ++ up xss


merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) | x > y = y : merge (x : xs) ys
                        | otherwise = x : merge xs (y : ys)

sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort xs = merge (sort firstHalf) (sort secondHalf)
  where
    len = length xs
    d = len `div` 2
    firstHalf = take d xs
    secondHalf = drop d xs
