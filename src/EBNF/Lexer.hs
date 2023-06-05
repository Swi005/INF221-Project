{-# LANGUAGE OverloadedStrings #-}

module EBNF.Lexer where


import Data.Void(Void)
import Data.Text(Text)

import Control.Monad (void)
import Control.Applicative(empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error


import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

-- | sc(space consumer) consumes spaces
--
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- | Wrapper for consuming spaces after a lexeme
--
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Wrapper that consumes spaces after a symbol
--
symbol :: String -> Parser String
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <|> char '\'' *> manyTill L.charLiteral (char '\'')

integer :: Parser Integer
integer = lexeme L.decimal

charClass :: Parser [Char]
charClass = between (char '[') (char ']') (manyTill L.charLiteral (char ']'))