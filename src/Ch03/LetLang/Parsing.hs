{-# LANGUAGE LambdaCase #-}
-- |

module Ch03.LetLang.Parsing where

import Prelude hiding (any, exp)
import Parsing2
import Data.Functor (($>), (<&>))
import Control.Applicative (Alternative((<|>)))
import Ch03.LetLang.Types
import Ch01.LcExp (identifier)

-- scanAndParse :: String -> Either String Program
-- scanAndParse s = let (_,res) = runParser scanner s
--                  in case res of
--                       Left e -> Left $ show e
--                       Right ts ->
--                         let (_,res') = runParser letLangParser ts
--                         in either (Left . show) Right res'

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
  | Equals
  | Ident String
  | Key String
  | Num Int
  deriving (Show, Eq)

keywords = ["if", "then", "else", "let", "in", "zero?"]

testProgram = "-(55, -(x,11))"

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
          char '=' $> Equals <|>
          num <|>
          keyOrIdent

num = many1 digit <&> Num . read


keyOrIdent = do
  head <- letter
  tail <-  many $ letter <|> digit
  let word = head : tail
  return $ ( if word `elem` keywords then Key else Ident ) word

type LetLangParser a = Parser Token a

letLangParser :: Parser Token Program
letLangParser = Program <$> exp

exp :: Parser Token Exp
exp = try $ diff <|> zero <|> ifExp <|> letExp <|> constExp <|> varExp

token :: Token -> LetLangParser Token
token t = satisfy ("token " <> show t) (== t)

tokens :: [Token] -> LetLangParser [Token]
tokens = traverse token

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

zero :: LetLangParser Exp
zero = do
  tokens [Key "zero?", ParenL]
  testedExp <- exp
  token ParenR
  pure $ Zero testedExp

ifExp :: LetLangParser Exp
ifExp = do
  token $ Key "if"
  predExp <- exp
  token $ Key "then"
  thenExp <- exp
  token $ Key "else"
  elseExp <- exp
  pure $ If predExp thenExp elseExp

letExp :: LetLangParser Exp
letExp = do
  token $ Key "let"
  varName <- ident
  token Equals
  defExp <- exp
  token $ Key "in"
  bodyExp <- exp
  pure $ Let varName defExp bodyExp
