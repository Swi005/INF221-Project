module EBNF.CheckGrammar where

import EBNF.EBNF
import Data.List
import Control.Monad
import Data.Either

-- | Check if all identifiers used in a grammar are defined
checkIdentifiersExists :: EBNF -> Either String EBNF
checkIdentifiersExists g@(Grammar rules) =
        let ids = map (\(Symbol id e) -> (id,e)) rules
        in if all (checkExpr $ map fst ids) $ map snd ids then Right g else Left "Error, found undefined identifier"--TODO: make error message better
        where
            checkExpr :: [Identifier]->Expression->Bool
            checkExpr ids (Sequence exprs) = all (checkExpr ids) exprs
            checkExpr ids (Optional expr) = checkExpr ids expr
            checkExpr ids (Or expr1 expr2) = checkExpr ids expr1 && checkExpr ids expr2
            checkExpr ids (Plus expr) = checkExpr ids expr
            checkExpr ids (Star expr) = checkExpr ids expr
            checkExpr ids (SymbolRef id) = id `elem` ids
            checkExpr ids (Terminal str) = True
            -- checkExpr ids (REGEX regex) = True

checkDuplicateRules :: EBNF -> Either String EBNF 
checkDuplicateRules g@(Grammar rules) =
    let ids = map (\(Symbol id e) -> id) rules
    in if length ids == length (nub ids) then Right g else Left ( "Error, found duplicate rules: " ++ (show $ nub ids))


-- | Run all checks on a grammar, return error if any else return
checkGrammar :: EBNF -> EBNF
checkGrammar x = case checkGrammarEither x of
    Left e -> error $ e
    Right ebnf -> ebnf


-- | Run all checks on a grammar, return string of errors if threre are any else return the grammar
checkGrammarEither :: EBNF -> Either String EBNF 
checkGrammarEither ebnf =
    let checks = [checkIdentifiersExists, checkDuplicateRules]
        res = map (\check -> check ebnf) checks
    in if all isRight res then Right ebnf else Left $ foldl (\s (Left r) -> s ++ ";" ++ r) "" $ filter isLeft res