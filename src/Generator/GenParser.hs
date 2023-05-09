module Generator.GenParser(generate) where

import Generator.ParseTree
import Generator.GenUtils

import EBNF.EBNF as G
import Text.Megaparsec (parse)
import Text.Format

-- | Generates a parser from an EBNF grammar
generate :: EBNF -> String
generate (Grammar rules ) = header "Parser" ++ importTemplate' ++ nl ++ parseDef ++nl ++ nl ++ helperFuncs ++ nl ++ nl ++ concatMap generateRule rules

-- | Functions that are helpful for parsing
helperFuncs :: String
helperFuncs = "some2 p = liftM2 (:) p (some p)"

-- | Var names
vars :: [Char]
vars = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']


-- | Parses a rule from an EBNF grammar
generateRule :: Rule -> String
generateRule (G.Symbol id expr ) = 
    format "parse_{0} :: Parser (ParseTree String)  \n" [id] ++
    format "parse_{0} = do \n" [id] ++
    tab 1 ++ "a <- " ++ generateExpr expr ++ 
    nl ++
    tab 1 ++ format "return (Symbol \"{0}\" [a])" [id]++
    nl ++
    nl

-- | Generates a parser for a given expression
generateExpr :: Expression -> String
generateExpr (G.Sequence exprs) = "Symbol \"Sequence\" ["++ foo exprs ++ "]" --TODO: Fix this

generateExpr (G.Optional expr) = "(" ++ generateExpr expr ++ ") <|> pure ()"

generateExpr (G.Or expr1 expr2) = "(" ++ generateExpr expr1 ++ ") <|> (" ++ generateExpr expr2 ++ ")"

generateExpr (G.Plus expr) = "some $ " ++ generateExpr expr

generateExpr (G.Star expr) = "many $ " ++ generateExpr expr
--Base cases
generateExpr (G.SymbolRef id) = "parse_"++id

generateExpr (G.Terminal str) = "Literal $ symbol " ++ show str

foo :: [Expression]-> String
foo [] = ""
foo [x] = generateExpr x
foo (x:xs) = generateExpr x ++ "," ++ foo xs



-- | Definition for the Parser type 
parseDef :: String
parseDef = "type Parser = Parsec Void String \n"
