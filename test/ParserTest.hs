module Main (main) where

import System.IO

import Data.Void
import Data.Text

import Text.Megaparsec
import Text.Megaparsec.Error

import EBNF.Parser

main :: IO ()
main = runAllTests

testList = [1 .. 5]

test f = do 
        input <- readFile $ "test/examples/" ++ f
        --print input
        print $ generate $ parse ebnf f $ pack input
        --pPrint $ parse ebnf "foo" $ pack input
        --parseTest ebnf $ pack input

runAllTests = mapM_ (\t -> do  
            file <- ((readFile ("test/examples/ex" ++ show t)) >>= (return . pack))
            case runParser ebnf (show t) file of
                Left s -> error $ show s
                Right _ -> return ()
        ) testList