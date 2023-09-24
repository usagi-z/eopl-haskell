{-# LANGUAGE LambdaCase #-}
-- | 

module Ch01.LcExpStar where

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
  | Lambda [ String ] LcExp
  | App LcExp [ LcExp ]

instance Show LcExp where
  show = \case
    Identifier s -> s
    Lambda bs e -> printf "(lambda (%s) %s)" (unwords bs) (show e)
    App op args -> printf "(%s %s)" (show op) $ unwords $ map show args


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
     bs <- parens $ many1 identifier'
     spaces
     e <- lcExp
     spaces
     pure $ Lambda bs e

app :: Parser LcExp
app = try $ parens $
  do op <- lcExp
     spaces
     args <- many lcExp
     spaces
     pure $ App op args

-- lcExp :: Parser LcExp
-- lcExp = try identifier <|> try lam <|> app
lcExp :: Parser LcExp
lcExp =  identifier <|> lam <|> app

parser = lcExp
