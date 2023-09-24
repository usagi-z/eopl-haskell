{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

-- | 

module Parsing where
import Prelude hiding (any)
import Control.Applicative ( Alternative((<|>)) )
import Data.Char (isSpace, isLetter)
import GHC.Unicode (isDigit)

-- type Parser a = String -> Either ParseError a

data ParseError = ParseError
  { errExpected :: String
  , errFound    :: String
  }
  deriving Show

newtype Parser a = Parser {
  runParser :: String -> (String, Either ParseError a)
  }

any :: Parser Char
any = Parser $ \case
    (x:xs) -> (xs, Right x)
    [] -> ("", Left $ ParseError "any character" "the end of input")

eof :: Parser ()
eof = Parser $ \input -> case input of
  [] -> ("", Right ())
  (c: _) -> (input, Left $ ParseError "the end of input" [c])

parseError :: String -> String -> Parser a
parseError expected found = Parser (,Left $ ParseError expected found)


try :: Parser a -> Parser a
try p = Parser $ \state ->
  case runParser p state of
    (_newState, Left err) -> (state, Left err)
    success -> success

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc pred = try $ any >>= \c ->
                              if pred c
                              then pure c
                              else parseError desc [c]

char :: Char -> Parser Char
char c = satisfy ("char " <> [c]) (== c)

notChar :: Char -> Parser Char
notChar c = satisfy ("not " <> [c]) (/= c)

instance Functor Parser where
  fmap f pA = Parser $ \input ->
    let (rest, result) = runParser pA input
    in (rest, fmap f result)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (, pure x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parserF <*> parserA = Parser $ \input ->
    case runParser parserF input of
      (rest, Right f) ->  runParser (fmap f parserA) rest
      (rest, Left e) -> (rest, Left e)

andThen :: Parser a -> (a -> Parser b) -> Parser b
parserA `andThen` f = Parser $ \input ->
  case runParser parserA input of
    (rest, Right a) -> runParser (f a) rest
    (rest, Left e) -> (rest, Left e)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = andThen

instance Alternative Parser where
  -- empty :: Parser a
  -- empty = Parser $ const (mempty, Left $ ParseError "another branch" "empty Alternative")

  (<|>) :: Parser a -> Parser a -> Parser a
  pA <|> pB = Parser $ \s ->
    case runParser pA s of
      (s', Left err)
        | s' == s -> runParser pB s
        | otherwise -> (s', Left err)
      succ           -> succ

choice :: String -> [Parser a] -> Parser a
choice desc = foldr (<|>) noMatch
  where
    noMatch = parseError desc "no match"

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> pure []
many1 p = (:) <$> p <*> many p

sepBy, sepBy1 :: Parser s -> Parser a -> Parser [a]
sepBy s p = sepBy1 s p <|> pure []
sepBy1 s p = do
  first <- p
  rest <- many $ s >> p
  return $ first:rest

-- p = do
--   p <- char 'p'
--   spc <- char ' '
--   return [p, spc]

-- p' = sepBy (char ',') p

space = satisfy "space" isSpace
digit = satisfy "digit" isDigit
letter = satisfy "letter" isLetter


string ::  String -> Parser String
string = traverse char
spaces = many space
symbol s = string s <* spaces

between open close value = open *> value <* close
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

