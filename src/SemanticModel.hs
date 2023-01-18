module SemanticModel where
-- **Semantic Model Specifics**
-- All languages are type checked and all types will be inffered before evaluation
-- A language can be either dynamicaly typed (uses haskells own type inference) or statically typed
-- Should support both functional and procedural languages
--
-- Investigate using Generics to allow for both generics and OOP
-- 
-- Need to define language configs
--
-- **Universal(?) Language features**
--  -Functions
--  -Structs
--  -Exprs
--  -Types
-- 
-- **Language featurs needed for better extensibility**
--  -Bind lang features to Haskell funcs
--  -Wrapper for common/usefull Haskell 
--
-- **Semantic highlighting proposal**
--Use "[]" to assign semantics
-- > 
-- > [Type]
-- > string = [Char]*
-- > [Type] 
-- > types = [Bool] "Bool" | [Int] "Int" | string | "Func"
-- > 
-- > var_declaration = types identifier
-- >






--Very Basic Type System
--Supports Ints, Bools, Floats, Chars
--Uses Haskells own types
data Types = BasicTypes | None 
data BasicTypes = Int Int | Bool Bool | Float Float | Char Char

data Identifier = String