{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module Ch03.LetLangExtended.Parsing where

import Prelude hiding (any, exp)
import Parsing2
import Data.Functor (($>), (<&>))
import Control.Applicative (Alternative((<|>)))
import Ch03.LetLangExtended.Types

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

keywords = ["if", "then", "else", "let", "in", "cond", "end", "unpack"]

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
      condExp <|>
      unpackExp <|>
      op1 <|>
      op2 <|>
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

op1 :: LetLangParser Exp
op1 = try $ do
  opName <- ident
  Op1 opName <$> parenthesized exp

op2 :: LetLangParser Exp
op2 = try $ do
  opName <- ident
  ops <- parenthesized $ do
    operandA <- exp
    token Comma
    operandB <- exp
    pure (operandA, operandB)
  pure $ uncurry (Op2 opName) ops

opN :: LetLangParser Exp
opN = try $ do
  opName <- ident
  ops <- parenthesized $ sepBy (token Comma) exp
  pure $ OpN opName ops

ifExp :: LetLangParser Exp
ifExp = do
  key "if"
  predExp <- exp
  key "then"
  thenExp <- exp
  key "else"
  elseExp <- exp
  pure $ If predExp thenExp elseExp

condExp :: LetLangParser Exp
condExp = try $ do
  key "cond"
  condPairs <- many1 $ do
    condition <- exp
    token CondArrow
    body <- exp
    pure (condition, body)
  key "end"
  pure $ Cond condPairs

letExp :: LetLangParser Exp
letExp = do
  key "let"
  defPairs <- many1 $ do
    varName <- ident
    token Equals
    defExp <- exp
    pure (varName, defExp)
  key "in"
  bodyExp <- exp
  pure $ Let defPairs bodyExp

unpackExp :: LetLangParser Exp
unpackExp = do
  key "unpack"
  idents <- many ident
  token Equals
  listExp <- exp
  key "in"
  body <- exp
  pure $ Unpack idents listExp body
