module EBNF.CheckGrammar where
import EBNF.EBNF


-- | Check if all identifiers used in a grammar are defined
checkIdentifiersExists :: EBNF -> Bool
checkIdentifierExists (Grammar rules) = 
    let ids = map (\(Symbol id e) -> (id,e)) rules
    in all (checkExpr $ fst ids) $ snd rules
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


