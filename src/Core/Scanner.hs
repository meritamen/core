{-# LANGUAGE OverloadedStrings #-}

module Core.Scanner
  ( Parser
  , symbol
  , braces
  , parens
  , angles
  , comma
  , dot
  , backSlash
  , rightArrow
  , semicolon
  , equal
  , hyphen
  , integer
  , identifier
  , operators
  , keywords
  ) where

import           Control.Monad              (void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, many, (<|>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

comma :: Parser ()
comma = void $ symbol ","

dot :: Parser ()
dot = void $ symbol "."

backSlash :: Parser ()
backSlash = void $ symbol "\\"

rightArrow :: Parser ()
rightArrow = void $ symbol "->"

semicolon :: Parser ()
semicolon = void $ symbol ";"

equal :: Parser ()
equal = void $ symbol "="

hyphen :: Parser ()
hyphen = void $ symbol "-"

integer :: Parser Int
integer = lexeme L.decimal

identifier :: Parser Text
identifier = lexeme $
  T.cons <$> letterChar <*> (T.pack <$> many (alphaNumChar <|> char '_'))

operators :: [Parser Text]
operators = map symbol ["==", "~=", ">=", "<=", "+", "-", "*", "/", ">", "<", "&", "|"]

keywords :: [Text]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]
