module Main (main) where

import System.IO

import Data.Void
import Data.Text

import Text.Megaparsec
import Text.Megaparsec.Error

import EBNF.Parser
import Generator.GenParser ( generate )

main :: IO ()
main = runAllTests
out = "test/out/"

testList = [1 .. 5]

test f = do 
        input <- readFile $ "test/examples/" ++ f
        --print input
        case parse ebnf f $ pack input of
                Left e -> error $ show e
                Right r ->writeFile ("output.hs" ) $ generate r
        --pPrint $ parse ebnf "foo" $ pack input
        --parseTest ebnf $ pack input


runAllTests = mapM_ (\t -> do  
            file <- ((readFile ("test/examples/ex" ++ show t)) >>= (return . pack))
            case runParser ebnf (show t) file of
                Left s -> error $ show s
                Right _ -> return ()
        ) testList