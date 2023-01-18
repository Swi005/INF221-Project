module EBNF where
--should use EBNF as defined in https://www.w3.org/TR/REC-xml/#sec-notation

data EBNF = [Rule]

data Rule = Terminal Identifier [Symbol] 
          | NonTerminal Identifier [Rule]


newtype Symbol = Symbol String 
type Identifier = String