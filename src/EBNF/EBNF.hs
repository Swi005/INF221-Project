module EBNF.EBNF where
import EBNF.REGEX
--should use EBNF as defined in https://www.w3.org/TR/REC-xml/#sec-notation

-- | AST for an EBNF grammar
newtype EBNF = Grammar [Rule] deriving (Show,Eq)


--Statements
data Rule = Symbol Identifier Expression
            deriving (Show,Eq)

-- | Exprs
data Expression
          = Sequence [Expression]
          | Optional Expression -- ( ? )
          | Or Expression Expression -- |
          | Plus Expression -- +
          | Star Expression -- *
          | SymbolRef Identifier -- <id>
          | Terminal String -- "string"
          | REGEX Regex --Regex
            deriving (Show,Eq)


type Identifier = String
