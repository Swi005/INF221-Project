module SemanticModel where
import SymbolTable
-- **Semantic Model Specifics**
-- All languages are type checked and all types will be inffered before evaluation
-- A language can be either dynamicaly typed (uses haskells own type inference) or statically typed
-- Should support both functional and procedural languages
--
-- Inspired by https://github.com/lcompilers/libasr/blob/main/src/libasr/ASR.asdl
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
--Maps to Haskells own types


--Should be an unique String

data Symbol = Program { 
        program_name::Identifier, 
        symbol_table::SymbolTable Symbol,
        dependencies::[Identifier], 
        body::[Stmt]
        }
    | Module {  
        module_name::Identifer,
        symbol_table :: SymbolTable Symbol,
        dependencies::[Identifier]
        }
    | Procedure {
        procedure_name :: Identifier,
        symbol_table::SymbolTable Symbol,
        params::[Expr],
        body::[Stmt]
        }
    | GenericProcedure { --TODO: This
        procedureName :: Identifier,
        symbol_table::SymbolTable Symbol
        }
    | Struct { --TODO: This
        struct_name :: Identifier,
        symbol_table::SymbolTable Symbol,
        members :: [Identifier]
        }
    | Variable {
        var_name :: identifier

        }


data Types = BasicTypes | None 
data BasicTypes = Int Int | Bool Bool | Float Float | Char Char
