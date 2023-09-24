{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleInstances #-}

-- | 

module Parsing2 where
import Prelude hiding (any)
import Control.Applicative ( Alternative((<|>)) )
import Data.Char (isSpace, isLetter)
import GHC.Unicode (isDigit)
import Data.Either (either)
import Data.Function (on)


data ParseError t
  = ParseError { errExpected :: Either String t
               , errFound    :: Either String t
               }
  -- deriving Show
instance Show t => Show (ParseError t) where
  show e = "Expected: " <> unpack e.errExpected <> "\n" <>
           "Found: " <> unpack e.errFound <> "\n"
    where
      unpack = either id show

parseError' e f = ParseError (pure e) (pure f)
parseError'' e f = ParseError (Left e) (pure f)

newtype Parser t a = Parser {
  runParser :: [t] -> ([t], Either (ParseError t) a)
  }

type ParserS = Parser Char

any :: Parser t t
any = Parser $ \case
    (x:xs) -> (xs, Right x)
    [] -> ([], Left $ ParseError (Left "any character")
                                 (Left "the end of input"))

eof :: Parser t ()
eof = Parser $ \input -> case input of
  [] -> ([], Right ())
  (x: _) -> (input, Left $ ParseError (Left "the end of input" ) $ Right x)

parseError :: Either String t -> Either String t -> Parser t a
parseError expected found = Parser (,Left $ ParseError expected found)


try :: Parser xs a -> Parser xs a
try p = Parser $ \state ->
  case runParser p state of
    (_newState, Left err) -> (state, Left err)
    success -> success

satisfy :: String -> (t -> Bool) -> Parser t t
satisfy desc pred = try $ any >>= \c ->
                              if pred c
                              then pure c
                              else parseError (Left desc) (Right c)


instance Functor (Parser t) where
  fmap f pA = Parser $ \input ->
    let (rest, result) = runParser pA input
    in (rest, fmap f result)

instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure x = Parser (, pure x)
  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  parserF <*> parserA = Parser $ \input ->
    case runParser parserF input of
      (rest, Right f) ->  runParser (fmap f parserA) rest
      (rest, Left e) -> (rest, Left e)

andThen :: Parser t a -> (a -> Parser t b) -> Parser t b
parserA `andThen` f = Parser $ \input ->
  case runParser parserA input of
    (rest, Right a) -> runParser (f a) rest
    (rest, Left e) -> (rest, Left e)

instance Monad (Parser t) where
  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  (>>=) = andThen

instance Eq t => Alternative (Parser t) where
  -- empty :: Parser t a
  -- empty = Parser $ const (mempty, Left $ ParseError "another branch" "empty Alternative")

  (<|>) :: Parser t a -> Parser t a -> Parser t a
  pA <|> pB = Parser $ \s ->
    case runParser pA s of
      (s', Left err)
        -- | s' == s -> runParser pB s
        | on (==) length s s' -> runParser pB s
        | otherwise -> (s', Left err)
      succ           -> succ

choice :: Eq t => String -> [Parser t a] -> Parser t a
choice desc = foldr (<|>) noMatch
  where
    noMatch = parseError (Left desc) (Left  "no match")

many, many1 :: Eq t => Parser t a -> Parser t [a]
many p = many1 p <|> pure []
many1 p = (:) <$> p <*> many p

sepBy, sepBy1 :: Eq t => Parser t s -> Parser t a -> Parser t [a]
sepBy s p = sepBy1 s p <|> pure []
sepBy1 s p = do
  first <- p
  rest <- many $ s >> p
  return $ first:rest

optional :: Eq t => Parser t a -> Parser t (Maybe a)
optional p = (Just <$> p) <|> pure Nothing


-- Char Parsers

char :: Char -> Parser Char Char
char c = satisfy ("char " <> [c]) (== c)

notChar :: Char -> Parser Char Char
notChar c = satisfy ("not " <> [c]) (/= c)

space = satisfy "space" isSpace
digit = satisfy "digit" isDigit
letter = satisfy "letter" isLetter


string ::  String -> Parser Char String
string = traverse char
spaces = many space
symbol s = string s <* spaces

between open close value = open *> value <* close
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")
