{-# LANGUAGE DerivingStrategies #-}
module Generator.ParseTree where

import Control.Monad

import Data.Text

import Text.Format
import Generator.GenUtils
import GHC.Prelude (Show)


--import qualified EBNF.EBNF as G


type Label = String

data ParseTree a = Symbol Label [ParseTree a] 
                | Literal a
                deriving (Show, Eq)

instance Functor ParseTree where
    fmap f (Literal a) = Literal (f a)
    fmap f (Symbol l kids) = Symbol l (fmap (fmap f) kids) 


instance Monad ParseTree where
    return = Literal
    (Literal a) >>= f = Literal $ f a
    (Symbol l kids) >>= f = Symbol l (fmap (>>= f) kids)

{-
instance Show ParseTree where
    show (Literal a) = show a
    --show (Symbol) 
-}