module Parser where 
import Data.Void
import Data.Text
import Control.Monad (void,liftM2)
import Control.Monad.Combinators.Expr
import Control.Applicative hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char 
import Text.Megaparsec.Error
import Generator.ParseTree
import EBNF.Lexer(symbol, lexeme)

type Parser = Parsec Void Text


some2 p = liftM2 (:) p (some p)

parse_letter :: Parser (ParseTree String)  
parse_letter =  return (Symbol "letter" [(((((((((((((((((((((((((((((((((((((((((((((((((((Literal $ symbol "A") <|> (Literal $ symbol "B")) <|> (Literal $ symbol "C")) <|> (Literal $ symbol "D")) <|> (Literal $ symbol "E")) <|> (Literal $ symbol "F")) <|> (Literal $ symbol "G")) <|> (Literal $ symbol "H")) <|> (Literal $ symbol "I")) <|> (Literal $ symbol "J")) <|> (Literal $ symbol "K")) <|> (Literal $ symbol "L")) <|> (Literal $ symbol "M")) <|> (Literal $ symbol "N")) <|> (Literal $ symbol "O")) <|> (Literal $ symbol "P")) <|> (Literal $ symbol "Q")) <|> (Literal $ symbol "R")) <|> (Literal $ symbol "S")) <|> (Literal $ symbol "T")) <|> (Literal $ symbol "U")) <|> (Literal $ symbol "V")) <|> (Literal $ symbol "W")) <|> (Literal $ symbol "X")) <|> (Literal $ symbol "Y")) <|> (Literal $ symbol "Z")) <|> (Literal $ symbol "a")) <|> (Literal $ symbol "b")) <|> (Literal $ symbol "c")) <|> (Literal $ symbol "d")) <|> (Literal $ symbol "e")) <|> (Literal $ symbol "f")) <|> (Literal $ symbol "g")) <|> (Literal $ symbol "h")) <|> (Literal $ symbol "i")) <|> (Literal $ symbol "j")) <|> (Literal $ symbol "k")) <|> (Literal $ symbol "l")) <|> (Literal $ symbol "m")) <|> (Literal $ symbol "n")) <|> (Literal $ symbol "o")) <|> (Literal $ symbol "p")) <|> (Literal $ symbol "q")) <|> (Literal $ symbol "r")) <|> (Literal $ symbol "s")) <|> (Literal $ symbol "t")) <|> (Literal $ symbol "u")) <|> (Literal $ symbol "v")) <|> (Literal $ symbol "w")) <|> (Literal $ symbol "x")) <|> (Literal $ symbol "y")) <|> (Literal $ symbol "z")])

parse_digit :: Parser (ParseTree String)  
parse_digit =  return (Symbol "digit" [(((((((((Literal $ symbol "0") <|> (Literal $ symbol "1")) <|> (Literal $ symbol "2")) <|> (Literal $ symbol "3")) <|> (Literal $ symbol "4")) <|> (Literal $ symbol "5")) <|> (Literal $ symbol "6")) <|> (Literal $ symbol "7")) <|> (Literal $ symbol "8")) <|> (Literal $ symbol "9")])

parse_symbol :: Parser (ParseTree String)  
parse_symbol =  return (Symbol "symbol" [(((((((((((((Literal $ symbol "[") <|> (Literal $ symbol "]")) <|> (Literal $ symbol "+")) <|> (Literal $ symbol "(")) <|> (Literal $ symbol ")")) <|> (Literal $ symbol "<")) <|> (Literal $ symbol ">")) <|> (Literal $ symbol "'")) <|> (Literal $ symbol "\"")) <|> (Literal $ symbol "=")) <|> (Literal $ symbol "|")) <|> (Literal $ symbol ".")) <|> (Literal $ symbol "*")) <|> (Literal $ symbol ";")])

parse_character :: Parser (ParseTree String)  
parse_character =  return (Symbol "character" [(((parse_letter) <|> (parse_digit)) <|> (parse_symbol)) <|> (Literal $ symbol "_")])

parse_identifier :: Parser (ParseTree String)  
parse_identifier =  return (Symbol "identifier" [Symbol "Sequence" [parse_letter,many $ ((parse_letter) <|> (parse_digit)) <|> (Literal $ symbol "_")]])

parse_terminal :: Parser (ParseTree String)  
parse_terminal =  return (Symbol "terminal" [Symbol "Sequence" [Literal $ symbol "'",parse_character,many $ parse_character,(Literal $ symbol "'") <|> (Literal $ symbol "\""),parse_character,many $ parse_character,Literal $ symbol "\""]])

parse_lhs :: Parser (ParseTree String)  
parse_lhs =  return (Symbol "lhs" [parse_identifier])

parse_rhs :: Parser (ParseTree String)  
parse_rhs =  return (Symbol "rhs" [Symbol "Sequence" [((parse_identifier) <|> (parse_terminal)) <|> (Literal $ symbol "["),parse_rhs,(Literal $ symbol "]") <|> (parse_rhs),(Literal $ symbol "*") <|> (parse_rhs),(Literal $ symbol "+") <|> (Literal $ symbol "("),parse_rhs,(Literal $ symbol ")") <|> (parse_rhs),Literal $ symbol "|",(parse_rhs) <|> (parse_rhs),Literal $ symbol " ",parse_rhs]])

parse_rule :: Parser (ParseTree String)  
parse_rule =  return (Symbol "rule" [Symbol "Sequence" [parse_lhs,Literal $ symbol "=",parse_rhs,Literal $ symbol ";"]])

parse_grammar :: Parser (ParseTree String)  
parse_grammar =  return (Symbol "grammar" [many $ parse_rule])

parse_regex :: Parser (ParseTree String)  
parse_regex =  return (Symbol "regex" [Symbol "Sequence" [Literal $ symbol "REGEX",Literal $ symbol "(",Literal $ symbol ")"]])

