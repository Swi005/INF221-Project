module EBNF where
--EBNF should follow ISO/IEC 14977

data EBNF = [Rule]

data Rule = Terminal Identifier [Symbol] 
          | NonTerminal Identifier [Rule]


newtype Symbol = Symbol String 
type Identifier = String