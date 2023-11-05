{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Ch04.ImplicitRefsLang.Parsing where

import           Ch04.ImplicitRefsLang.Types
import           Control.Applicative         (Alternative ((<|>)))
import           Data.Functor                (($>), (<&>))
import           Data.Maybe                  (isJust)
import           Parsing2
import           Prelude                     hiding (any, exp)

-- import Ch04.ExplicitRefsLang.Tests

scanAndParse :: String -> Either String Program
scanAndParse s = case runParser scanner s of
  (_, Left e) -> Left $ show e
  ([], Right ts) ->
    case runParser letLangParser ts of
      (_, Left e)     -> Left $ show e
      ([], Right exp) -> Right exp
      (xs, _)         -> Left $ "parser failed, leftover: " <> show xs
  (xs, _) -> Left $ "scanner failed, leftover: " <> xs

type Scanner a = Parser Char a

data Token
  = Minus
  | ParenL
  | ParenR
  | Comma
  | Semicolon
  | CondArrow
  | Equals
  | Ident String
  | Key String
  | Num Int
  deriving (Show, Eq)

keywords =
  [ "if",
    "then",
    "else",
    "let",
    "in",
    "end",
    "proc",
    "letrec",
    "set",
    "begin",
    "end"
  ]

testProgram1 = "-(55, -(x,11))"

testProgram2 = "zero?(-(55, -(x,11)))"

scanner :: Scanner [Token]
scanner = many $ do
  spaces
  t <- scanner'
  spaces
  return t

scanner' :: Scanner Token
scanner' =
  num
    <|> keyOrIdent
    <|> char '-' $> Minus
    <|> char '(' $> ParenL
    <|> char ')' $> ParenR
    <|> char ',' $> Comma
    <|> char '=' $> Equals
    <|> char ';' $> Semicolon

num = try $ do
  minus <- optional $ char '-'
  ds <- many1 digit
  let n = read ds
      s = if isJust minus then negate else id
  pure $ Num $ s n

keyOrIdent = do
  head <- letter
  tail <- many $ letter <|> digit <|> char '?'
  let word = head : tail
  return $ (if word `elem` keywords then Key else Ident) word

type LetLangParser a = Parser Token a

letLangParser :: Parser Token Program
letLangParser = Program <$> exp

exp :: Parser Token Exp
exp =
  diff
    <|> constExp
    <|> setExp
    <|> beginExp
    <|> ifExp
    <|> letRecExp
    <|> letExp
    <|> procExp
    <|> appExp
    <|> opN
    <|> varExp

token :: Token -> LetLangParser Token
token t = satisfy ("token " <> show t) (== t)

key :: String -> LetLangParser Token
key = token . Key

tokens :: [Token] -> LetLangParser [Token]
tokens = traverse token

parenthesized = between (token ParenL) (token ParenR)

number :: LetLangParser Int
number =
  try $
    any
      >>= \case
        Num i -> pure i
        x -> parseError (Left "a number") (Right x)

constExp :: LetLangParser Exp
constExp = do
  minus <- optional $ token Minus
  i <- number
  return $ Const $ case minus of
    Nothing -> i
    Just _  -> -i

varExp :: LetLangParser Exp
varExp =
  try $
    any
      >>= \case
        Ident s -> pure $ Var s
        x -> parseError (Left "a var") (Right x)

ident :: LetLangParser String
ident =
  try $
    any
      >>= \case
        (Ident s) -> pure s
        x -> parseError (Left "an identifier") (Right x)

diff :: LetLangParser Exp
diff = try $ do
  tokens [Minus, ParenL]
  minuend <- exp
  token Comma
  subtrahend <- exp
  token ParenR
  pure $ Diff minuend subtrahend

opN :: LetLangParser Exp
opN = try $ do
  opName <- ident
  ops <- parenthesized $ sepBy (token Comma) exp
  pure $ Op opName ops

ifExp :: LetLangParser Exp
ifExp = do
  key "if"
  predExp <- exp
  key "then"
  thenExp <- exp
  key "else"
  elseExp <- exp
  pure $ If predExp thenExp elseExp

letExp :: LetLangParser Exp
letExp = try $ do
  key "let"
  defPairs <- many1 $ do
    varName <- ident
    token Equals
    defExp <- exp
    pure (varName, defExp)
  key "in"
  bodyExp <- exp
  pure $ Let defPairs bodyExp

procExp :: LetLangParser Exp
procExp = try $ do
  key "proc"
  var <- many1 ident
  body <- exp
  pure $ ProcExp var body

appExp :: LetLangParser Exp
appExp = try $
  parenthesized $
    do
      rator <- exp
      rands <- many exp
      pure $ App rator rands

letRecExp :: LetLangParser Exp
letRecExp = try $ do
  key "letrec"
  defs <- many1 $ do
    procName <- ident
    vars <- parenthesized $ sepBy1 (token Comma) ident
    token Equals
    procBody <- exp
    pure (procName, vars, procBody)
  key "in"
  letBody <- exp
  pure $ Letrec defs letBody

setExp :: LetLangParser Exp
setExp = try $ do
  key "set"
  varName <- ident
  token Equals
  valExp <- exp
  pure $ SetExp varName valExp

beginExp :: LetLangParser Exp
beginExp = try $ do
  key "begin"
  exps <- sepBy (token Semicolon) exp
  key "end"
  return $ BeginExp exps
