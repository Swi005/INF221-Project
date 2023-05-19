{-# LANGUAGE OverloadedStrings #-}
-- | Parses regex and generates a regex AST, 
-- | Only supports some regex features.
-- Supported features:
-- -Character classes:
-- --Character ranges
-- -- \d
-- -- \s
-- -Assertions
-- -- ^
-- -- $
-- -Quantifiers
-- -- *
-- -- +
-- -- ?
-- -- x{n}
module EBNF.REGEX(Regex, regex) where

import System.IO

import Data.Void
import Data.Text

import Control.Monad (void,liftM2)
import Control.Monad.Combinators.Expr
import Control.Applicative hiding (many, some)

import Text.Megaparsec
import Text.Megaparsec.Char 
import Text.Megaparsec.Error
import Text.Pretty.Simple

import EBNF.Lexer as Lexer
import EBNF.EBNF as AST
import qualified EBNF.Lexer as Lexer

type Parser = Parsec Void Text

newtype Regex = Regex [RegexPart]

data RegexPart = Quantifier Quantifier
                | Assertion Assertion
                | CharacterClass CharacterClass

data CharacterClass = Range [Char]
                    | Digit
                    | Whitespace
data Assertion = Any Regex
                | End Regex


data Quantifier = Star Regex
                | Plus Regex
                | Question Regex
                | Take Regex Int


regex :: Parser Regex
regex = Regex <$> many regexPart

regexPart ::Parser RegexPart
regexPart = try (Quantifier <$> quantifier)
            <|> try (Assertion <$> assertion)
            <|> try (CharacterClass <$> characterClass)

quantifier :: Parser Quantifier
quantifier = try (Star <$> (regex <* symbol "*"))
            <|> try (Plus <$> (regex <* symbol "+"))
            <|> try (Question <$> (regex <* symbol "?"))
            <|> try (Take <$> (regex <* symbol "{") <*> (Lexer.integer <* symbol "}"))

assertion :: Parser Assertion
assertion = try (Any <$> (symbol "^" *> regex))
            <|> try (End <$> (symbol "$" *> regex))

characterClass :: Parser CharacterClass
characterClass = try (Range <$> Lexer.charClass)
                <|> try (Whitespace <$ symbol "\\s")
                <|> try (Digit <$ symbol "\\d")
                