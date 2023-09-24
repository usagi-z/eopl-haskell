{-# LANGUAGE LambdaCase #-}
-- | 

module JSONParsing where

import Parsing
import qualified Data.Map.Strict as M
import Data.Char (toLower)
import Text.Printf (printf)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import Prelude hiding (any)
import Control.Monad (join)
import Data.Functor ((<&>))

data JValue = JObject (M.Map String JValue)
            | JArray [JValue]
            | JString String
            | JNumber Double
            | JBool Bool
            | JNull


instance Show JValue where
  show = \case
    JNull     -> "null"
    JBool b   -> toLower <$> show b
    JNumber n -> show n
    JString s -> show s
    JArray  a -> show a
    JObject o -> printf "{%s}" $
      intercalate ", " [printf "%s: %s" (show k) (show v)
                       | (k,v) <- M.toList o
                       ]


jsonNumber :: Parser Double
jsonNumber = read <$> many1 digit

jsonNull :: Parser String
jsonNull = string "null"

jsonBool :: Parser Bool
jsonBool = do b <- string "true" <|> string "false"
              case b of
                "true" -> pure True
                "false" -> pure False
                _ -> parseError "boolean" "something else" -- TODO: how to get the found string?

escapeSequence :: Parser String
escapeSequence = do b <- char '\\'
                    c <- any
                    pure [b, c]

nonQuoteString :: Parser String
nonQuoteString = many1 $ notChar '"'

jsonString :: Parser String
jsonString = do char '"'
                s <- many $ escapeSequence <|> nonQuoteString
                char '"'
                pure $ join s

jsonElement :: Parser JValue
jsonElement = spaces *> jsonValue <* spaces
                 
jsonArray :: Parser [JValue]
jsonArray = brackets $ sepBy (char ',') jsonElement -- bug: spaces should be optional, maybe write a lexer first

jsonMember :: Parser (String, JValue)
jsonMember = do key <- spaces *> jsonString <* spaces
                char ':'
                el <- jsonElement
                pure (key, el)

jsonMembers :: Parser [(String, JValue)]
jsonMembers = sepBy (char ',') jsonMember

jsonObject :: Parser (M.Map String JValue)
jsonObject = braces jsonMembers <&> M.fromList


jsonValue :: Parser JValue
jsonValue = JString <$> jsonString <|>
            JBool <$> jsonBool <|>
            JNumber <$> jsonNumber <|>
            JNull <$ jsonNull <|>
            JArray <$> jsonArray <|>
            JObject <$> jsonObject

json = jsonElement
