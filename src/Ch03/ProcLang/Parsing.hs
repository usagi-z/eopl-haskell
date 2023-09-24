{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module Ch03.ProcLang.Parsing where

import Prelude hiding (any, exp)
import Parsing2
import Data.Functor (($>), (<&>))
import Control.Applicative (Alternative((<|>)))
import Ch03.ProcLang.Types
-- import Ch03.ProcLang.Tests

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

keywords = ["if", "then", "else", "let", "in", "cond", "end", "proc", "procd", "letproc"]

testProgram1 = "-(55, -(x,11))"

testProgram2 = "zero?(-(55, -(x,11)))"

scanner :: Scanner [Token]
scanner = many $ do
  spaces
  t <- scanner'
  spaces
  return t

scanner' :: Scanner Token
scanner' = char '-' $> Minus <|>
          char '(' $> ParenL <|>
          char ')' $> ParenR <|>
          char ',' $> Comma <|>
          condArrow <|>
          char '=' $> Equals <|>
          num <|>
          keyOrIdent

num = many1 digit <&> Num . read

condArrow = try $ string "==>" $> CondArrow

keyOrIdent = do
  head <- letter
  tail <-  many $ letter <|> digit <|> char '?'
  let word = head : tail
  return $ ( if word `elem` keywords then Key else Ident ) word

type LetLangParser a = Parser Token a

letLangParser :: Parser Token Program
letLangParser = Program <$> exp

exp :: Parser Token Exp
exp = try $ diff <|>
      ifExp <|>
      letExp <|>
      procExp <|>
      procDExp <|> -- letProcExp <|>
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
diff = do
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

procDExp :: LetLangParser Exp
procDExp = try $ do
  key "procd"
  vars <- parenthesized $ sepBy1 (token Comma) ident
  body <- exp
  pure $ ProcDExp vars body

appExp :: LetLangParser Exp
appExp = try $ parenthesized $ -- App <$> exp <*> many exp
  do rator <- exp
     rands <- many exp
     pure $ App rator rands

-- letProcExp :: LetLangParser Exp
-- letProcExp = try $ do
--   key "letproc"
--   procName <- ident
--   token Equals
--   var <- parenthesized ident
--   procBody <- exp
--   key "in"
--   letBody <- exp
--   pure $ LetProc procName var procBody letBody
