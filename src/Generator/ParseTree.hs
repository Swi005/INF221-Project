{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
module Generator.ParseTree where

import Control.Monad


import Generator.GenUtils
import Control.Applicative


--import qualified EBNF.EBNF as G


type Label = String

data ParseTree a = Symbol Label [ParseTree a]
                | Literal a
                deriving (Show, Eq)

instance Functor ParseTree where
    fmap f (Literal a) = Literal (f a)
    fmap f (Symbol l kids) = Symbol l (fmap (fmap f) kids)
{-
instance Foldable ParseTree where
    foldr :: (a -> b -> b) -> b -> ParseTree a -> b
    foldr f b (Literal a) = f a b
    foldr f b (Symbol _ (x:xs)) = foldr f (foldr f b x) xs




instance Traversable ParseTree where
    traverse f (Symbol label x) = Symbol label <$> Prelude.foldr (\x r -> liftA2 (:) (g x) r) (pure [])
    traverse f (Literal a) = Literal <$> f a

instance Monad ParseTree where
    return = Literal
    (Literal a) >>= f = Literal $ f a
    (Symbol l kids) >>= f = Symbol l (fmap (>>= f) kids)

-}