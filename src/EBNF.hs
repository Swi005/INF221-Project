module EBNF where
--should use EBNF as defined in https://www.w3.org/TR/REC-xml/#sec-notation

data EBNF = [Rule]

data Rule = Symbol Identifier [Literal] 
          | Expression Identifier [Production]
data Production
          = Optional Rule
          | AND Rule RUle
          | OR Rule Rule
          | Diff Rule Rule
          | Plus Rule
          | Star Rule
          | Any [Symbol]


newtype Literal = Literal String 
type Identifier = String

data 