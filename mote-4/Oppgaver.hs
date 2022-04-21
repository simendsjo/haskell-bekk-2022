{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Functors where

import Prelude hiding (fmap)

class MyFunctor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap f [] = []
    fmap f (x : xs) = f x : fmap f xs

-- | OPPGAVE : implementer MyFunctor Maybe
-- >>> fmap (+1) (Just 1)
-- Just 2
instance MyFunctor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- | OPPGAVE : implementer MyFunctor (Either e)
-- >>> fmap (+1) (Right 1)
-- Right 2
-- >>> fmap (+1) (Left 1)
-- Left 1
instance MyFunctor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left a) = Left a
    fmap f (Right a) = Right (f a)

data OneOrTwo a = One a | Two a a
    deriving (Show)

-- | OPPGAVE : implementer MyFunctor OneOrTwo
-- >>> fmap (+1) (Two 1 2)
-- Two 2 3
-- >>> fmap id (One 1)
-- One 1
instance MyFunctor OneOrTwo where
  fmap :: (a -> b) -> OneOrTwo a -> OneOrTwo b
  fmap f (One a) = One (f a)
  fmap f (Two x y) = Two (f x) (f y)

data RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a
    deriving (Show)

-- | OPPGAVE : implementer MyFunctor for RemoteData
-- >>> fmap reverse (Success "ABC")
-- Success "CBA"
-- >>> fmap reverse NotAsked
-- NotAsked
instance MyFunctor (RemoteData e) where
  fmap :: (a -> b) -> RemoteData e a -> RemoteData e b
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e
  fmap _ NotAsked = NotAsked
  fmap _ Loading = Loading

-- | OPPGAVE : bruk det du har lært om functors til å gjøre om dataStuff til det som står i testen.
-- det finnes allerede even :: Int -> Bool
-- >>> dataStuffIsEven
-- [NotAsked,Loading,Failure "Gikk galt",Success True]
dataStuff :: [RemoteData String Int]
dataStuff =
    [ NotAsked
    , Loading
    , Failure "Gikk galt"
    , Success 0
    ]

dataStuffIsEven :: [RemoteData String Bool]
dataStuffIsEven = fmap (fmap even) dataStuff
