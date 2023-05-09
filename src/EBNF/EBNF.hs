module EBNF.EBNF where
--should use EBNF as defined in https://www.w3.org/TR/REC-xml/#sec-notation

newtype EBNF = Grammar [Rule] deriving (Show,Eq)


--Statements
data Rule = Symbol Identifier Expression
            deriving (Show,Eq)

--Exprs
data Expression
          = Sequence [Expression]
          | Optional Expression -- ( ? )
          | Or Expression Expression -- |
          | Plus Expression -- +
          | Star Expression -- *
          | SymbolRef Identifier -- <id>
          | Terminal String -- "string"
            deriving (Show,Eq)


--newtype Terminal = Terminal String deriving (Show,Eq)

type Identifier = String
