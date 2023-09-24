
module Ch02.PolishPrefixNotation where

data PrefixToken = Num Int | Minus

type PrefixList = [PrefixToken]

data PrefixExp = L Int | D PrefixExp PrefixExp
  deriving (Show)

pList = [Minus, Minus, Num 3, Num 2, Minus, Num 4, Minus, Num 12, Num 7]

parsePrefixList :: PrefixList -> PrefixExp
parsePrefixList = fst . go
  where
    go (Minus:(Num i):(Num j):xs) = (D (L i) (L j), xs)
    go (Minus:xs) = let (l,rest) = go xs
                        (r,rest') = go rest
                    in (D l r, rest')
    go ((Num i):xs) = (L i, xs)
