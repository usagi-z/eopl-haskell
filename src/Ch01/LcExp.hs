{-# LANGUAGE LambdaCase #-}
-- | 

module Ch01.LcExp where

import Parsing
import qualified Data.Map.Strict as M
import Data.Char (toLower)
import Text.Printf (printf)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import Prelude hiding (any)
import Control.Monad (join)
import Data.Functor ((<&>))

data LcExp
  = Identifier String
  | Lambda String LcExp
  | App LcExp LcExp

instance Show LcExp where
  show = \case
    Identifier s -> s
    Lambda b e -> printf "(lambda (%s) %s)" b (show e)
    App op arg -> printf "(%s %s)" (show op) (show arg)


identifier' :: Parser String
identifier' = do
  spaces
  hd <- many1 letter
  tl <- many (digit <|> letter)
  spaces
  pure (hd <> tl)

identifier :: Parser LcExp
identifier = Identifier <$> identifier'

lam :: Parser LcExp
lam = try $ parens $
  do symbol "lambda"
     b <- parens identifier'
     spaces
     e <- lcExp
     pure $ Lambda b e

app :: Parser LcExp
app = try $ parens $
  do op <- lcExp
     spaces
     arg <- lcExp
     pure $ App op arg

-- lcExp :: Parser LcExp
-- lcExp = try identifier <|> try lam <|> app
lcExp :: Parser LcExp
lcExp =  identifier <|> lam <|> app

parser = lcExp
