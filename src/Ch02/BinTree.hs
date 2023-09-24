-- | 

module Ch02.BinTree where
import Data.List (sort)



data BinTree = L Int | T String BinTree BinTree
 deriving Show



-- 2.24

-- binTreeToList :: BinTree -> [Int]
-- binTreeToList (L i) = []
-- binTreeToList (T s l r) = let lLst = binTreeToList l
--                               rLst = binTreeToList r
--                            in lLst <> [i] <> rLst

t1 = T "foo" (L 2) (L 3)
t2 = T "bar" (L (-1)) t1
t3 = T "baz" t2 (L 1)


leafSum :: BinTree -> Int
leafSum (L i) = i
leafSum (T _ l r) = leafSum l + leafSum r

maxInterior :: BinTree -> String
maxInterior = snd . mi 
mi (L i) = (i,"")
mi (T s l r) = let l'@(lSum,_) = mi l
                   r'@(rSum,_) = mi r
                   currentSum = rSum + lSum
               in maximum [(currentSum,s), l', r']
