module Generator.GenUtils(
    tab,
    nl,
    header,
    importTemplate,
    importTemplate'
    ) where

-- | Function that generates n amount of tabs
-- 
tab :: Int-> String
tab n = replicate n '\t'


-- | Funcion that genereates a newline
nl :: String
nl = "\n"


-- | Generates a haskell module header
--
header :: String -> String
header moduleName = "module " ++ moduleName ++ " where \n"

-- | Generates module import statements
importTemplate :: [String] -> String
importTemplate = concatMap (\m -> "import " ++ m ++ "\n")

-- | Generates most common module import statements for parsing
-- Imports: 
--  -Data.Void,
--  -Data.Text,
--  -Control.Monad (void)
--  -Control.Monad.Combinators.Expr
--  -Control.Applicative hiding (many, some)
--  -Text.Megaparsec
--  -Text.Megaparsec.Char
--  -Text.Megaparsec.Error
importTemplate' :: String
importTemplate' = importTemplate [
                    "Data.Void",
                    "Data.Text",
                    "Control.Monad (void,liftM2)",
                    "Control.Monad.Combinators.Expr",
                    "Control.Applicative hiding (many, some)",
                    "Text.Megaparsec",
                    "Text.Megaparsec.Char ",
                    "Text.Megaparsec.Error",
                    "Generator.ParseTree",
                    "EBNF.Lexer(symbol, lexeme)"
                ]
