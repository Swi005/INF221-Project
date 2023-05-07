{-# LANGUAGE OverloadedStrings #-}
module EBNF.Parser where

import System.IO

import Data.Void
import Data.Text

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Applicative hiding (many, some)

import Text.Megaparsec
import Text.Megaparsec.Char 
import Text.Megaparsec.Error
import Text.Pretty.Simple

import EBNF.Lexer as Lexer
import EBNF.EBNF as AST

-- | Parser a EBNF grammar and returns a EBNF 
--
parseFromFile file = runParser ebnf file <$> ((readFile file) >>= (return . pack))

ebnf :: Parser EBNF
ebnf = do
        sc
        rules <- many symbol'
        sc
        eof
        return (Grammar rules)

symbol' :: Parser Rule
symbol' = try(do
        id <- lexeme identifier
        e <- between (symbol "=") (symbol ";") expr'
        sc
        return (Symbol id e)
    )

expr' :: Parser Expression 
expr' = makeExprParser terms operatorTable <?> "expression"


terms = choice [parens', sequence', terminal, symbolRef'] <?> "term"


operatorTable :: [[Operator Parser Expression]]
operatorTable = [
        [
            binary "|" Or
        ],
        [
            postfix "+" Plus,
            postfix "?" Optional,
            postfix "*" Star
        ]
    ]

parens' :: Parser Expression
parens' = try $ between (symbol "(") (symbol ")") expr'


sequence' :: Parser Expression
sequence' = try (do
            sc
            rs <- between (symbol "[") (symbol "]") (some expr')
            sc
            return (Sequence rs)
    )

symbolRef' :: Parser Expression
symbolRef' = try(do
        sc
        id <-identifier
        sc
        return (SymbolRef id)
    )

terminal :: Parser Expression
terminal = try(do 
        sc
        str <- lexeme $ stringLiteral
        sc
        return (Terminal str)
    )

identifier :: Parser Identifier
identifier = try(do
            id <- some $ alphaNumChar
            sc
            return id
        )


binary :: Text -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)