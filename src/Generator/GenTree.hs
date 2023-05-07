-- | Module for generating parse trees
--   All parse trees will look like:
--   data ParseTree = Lit String
--                  | SYMBOL_NAME [ParseTree]
module Generator.GenTree (genTree, genTree') where

import Data.Text

import Text.Format
import Generator.GenUtils


import EBNF.EBNF as G


-- | Generates the a Haskell module containg the parse tree
-- 
genTree :: G.EBNF -> String
genTree grammar = genTree' "ParseTree"

genTree' :: String -> G.EBNF -> String
genTree' name grammar = undefined


-- | Generates a haskell datastructure that represents the parse tree
-- 
genData :: [G.Symbol]->String
genData [] _ = error "No symbols in grammar"
genData (sym:xs) = (genDataTop "ParseTree" 1 sym) ++ genData' 5 xs

genData' :: [G.Symbol]->Int->String
genData' (s:xs) t= genDataTop


genDataTop name t sym = tab t ++ format "data {0} = {1} \n" [name, sym]


-- | Haskell module header 
-- 
moduleHeader :: String -> String
moduleHeader name = format "module {0} where \n" [name]

