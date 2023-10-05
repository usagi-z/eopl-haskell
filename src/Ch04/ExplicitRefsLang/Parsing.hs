{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module Ch04.ExplicitRefsLang.Parsing where

import Prelude hiding (any, exp)
import Parsing2
import Data.Functor (($>), (<&>))
import Control.Applicative (Alternative((<|>)))
import Ch04.ExplicitRefsLang.Types
import Data.Maybe (isJust)
-- import Ch04.ExplicitRefsLang.Tests

scanAndParse :: String -> Either String Program
scanAndParse s = case runParser scanner s of
                      (_,Left e) -> Left $ show e
                      ([], Right ts) ->
                        case runParser letLangParser ts of
                          (_,Left e) -> Left $ show e
                          ([], Right exp) -> Right exp
                          (xs, _) -> Left $ "parser failed, leftover: " <> show xs
                      (xs, _) -> Left $ "scanner failed, leftover: " <> xs

type Scanner a = Parser Char a

data Token
  = Minus
  | ParenL
  | ParenR
  | Comma
  | CondArrow
  | Equals
  | Ident String
  | Key String
  | Num Int
  deriving (Show, Eq)

keywords = ["if", "then", "else", "let", "in", "end", "proc", "letrec"]

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
  num <|>
  keyOrIdent <|>
  char '-' $> Minus <|>
  char '(' $> ParenL <|>
  char ')' $> ParenR <|>
  char ',' $> Comma <|>
  char '=' $> Equals

num = try $ do
  minus <- optional $ char '-'
  ds <- many1 digit
  let n = read ds
      s = if isJust minus then negate else id
  pure $ Num $ s n


keyOrIdent = do
  head <- letter
  tail <-  many $ letter <|> digit <|> char '?'
  let word = head : tail
  return $ ( if word `elem` keywords then Key else Ident ) word

type LetLangParser a = Parser Token a

letLangParser :: Parser Token Program
letLangParser = Program <$> exp

exp :: Parser Token Exp
exp = diff <|>
      ifExp <|>
      letRecExp <|>
      letExp <|>
      procExp <|>
      appExp <|>
      opN <|>
      constExp <|>
      varExp

token :: Token -> LetLangParser Token
token t = satisfy ("token " <> show t) (== t)

key :: String -> LetLangParser Token
key = token . Key

tokens :: [Token] -> LetLangParser [Token]
tokens = traverse token

parenthesized = between (token ParenL) (token ParenR)

constExp :: LetLangParser Exp
constExp = try $ any >>=
  \case
    Num i -> pure $ Const i
    x -> parseError (Left "a constant") (Right x)

varExp :: LetLangParser Exp
varExp = try $ any >>=
  \case
    Ident s -> pure $ Var s
    x -> parseError (Left "a var") (Right x)

ident :: LetLangParser String
ident = try $ any >>=
  \case
    (Ident s) -> pure s
    x -> parseError (Left "an identifier") (Right x)

diff :: LetLangParser Exp
diff = try $ do
  tokens [ Minus, ParenL ]
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
  vars <- parenthesized $ sepBy1 (token Comma) ident
  body <- exp
  pure $ ProcExp vars body

appExp :: LetLangParser Exp
appExp = try $ parenthesized $ -- App <$> exp <*> many exp
  do rator <- exp
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

newrefExp :: LetLangParser Exp
newrefExp = try $ do
  key "newref"
  val <- parenthesized exp
  pure $ NewrefExp val

derefExp :: LetLangParser Exp
derefExp = try $ do
  key "deref"
  val <- parenthesized exp
  pure $ DerefExp val

setrefExp :: LetLangParser Exp
setrefExp = try $ do
  key "setref"
  (refexp, valexp) <- parenthesized $ do
    refexp <- exp
    token Comma
    valexp <- exp
    pure (refexp, valexp)
  pure $ SetrefExp refexp valexp
