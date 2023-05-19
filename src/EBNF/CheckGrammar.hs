module EBNF.CheckGrammar where

import EBNF.EBNF


-- | Check if all identifiers used in a grammar are defined
checkIdentifiersExists :: EBNF -> Either EBNF
checkIdentifierExists (Grammar rules) =
        let ids = map (\(Symbol id e) -> (id,e)) rules
        in if all (checkExpr $ fst ids) $ snd ids then rules else Left "Error, found undefined identifier"--TODO: make error message better
        where
            checkExpr :: [Identifier]->Expression->Bool
            checkExpr ids (Sequence exprs) = all (checkExpr ids) exprs
            checkExpr ids (Optional expr) = checkExpr ids expr
            checkExpr ids (Or expr1 expr2) = checkExpr ids expr1 && checkExpr ids expr2
            checkExpr ids (Plus expr) = checkExpr ids expr
            checkExpr ids (Star expr) = checkExpr ids expr
            checkExpr ids (SymbolRef id) = id `elem` ids
            checkExpr ids (Terminal str) = True
            checkExpr ids (REGEX regex) = True

checkDuplicateRules :: EBNF -> Either EBNF
checkDuplicateRules (Grammar rules) =
    let ids = map (\(Symbol id e) -> id) rules
    in if length ids == length (nub ids) then rules else Left "Error, found duplicate rules: " ++ show $ instersect ids (nub ids)


-- | Run all checks on a grammar, return error if any else return
checkGrammar :: EBNF -> EBNF
checkGrammar = case checkGrammarEither of
    Left error -> error $ error ebnf
    Right ebnf -> ebnf


-- | Run all checks on a grammar, return string of errors if threre are any else return the grammar
checkGrammarEither :: EBNF -> Either EBNF
checkGrammarEither ebnf =
    let checks = [checkIdentifiersExists, checkDuplicateRules]
        res = map (\check -> check ebnf) checks
    in if all isRight res then Right ebnf else foldM (\s r -> s ++ ";" ++ r) "" $ filter isLeft res