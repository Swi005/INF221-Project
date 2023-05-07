module GenParser(generate) where

import Generator.ParseTree
import Generator.GenUtils

import EBNF.EBNF as G
import Text.Megaparsec (parse)
import Text.Format

-- | Generates a parser from an EBNF grammar
generate :: EBNF -> String
generate (Grammar rules ) = header "Parser" ++ importTemplate' ++ nl ++ parseDef ++nl ++ nl ++ concatMap generateRule rules


-- | Parses a rule from an EBNF grammar
generateRule :: Rule -> String
generateRule (G.Symbol id expr ) = undefined
    format "parse{0} = do \n" [id] ++
    tab 1 ++ "a <- " ++ generateExpr expr ++ 
    nl ++
    tab 1 ++ format "return (Symbol {0} [])" [id]++
    nl ++
    nl


-- | Generates a parser for a given expression
generateExpr :: Expression -> String
generateExpr (G.Sequence exprs) = ""

generateExpr (G.Optional expr) = "(" ++ generateExpr expr ++ ") <|> pure ()"

--Todo: Fix this
generateExpr (G.Or expr1 expr2) = "(" ++ generateExpr expr1 ++ ") <|> (" ++ generateExpr expr2 ++ ")"

generateExpr (G.Plus expr) = "many1 $ " ++ generateExpr expr

generateExpr (G.Star expr) = "many $ " ++ generateExpr expr

generateExpr (G.SymbolRef id) = "id"

generateExpr (G.Terminal str) = "string " ++ show str

parseDef :: String
parseDef = "type Parser = Parsec Void String \n"