{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

-- Kinds

-- Gi eksempler på en type/typekonstuktør i stdlib og en du lager selv med disse kindene :

-- *

-- * -> *

-- * -> * -> *

-- (* -> * -> *) -> *    (kun lage selv)

-- Type classes

-- Vi har en klasse for typekonstruktører som kan gjøres om til lister

class ListLike f where
    toList :: f a -> [a]

-- Prøv å lag instanser for:
instance ListLike Maybe where
  toList (Just x) = [x]
  toList _ = []

data Identity a = Identity a

instance ListLike Identity where
  toList (Identity x) = [x]

-- (,) a
-- Either a
-- []
data Const a b = Const a

-- Lag en class for typer sånn at vi kan swappe typeparameterene

class Swap f where
  swap :: f a b -> f b a

instance Swap Either where
  swap (Left x) = Right x
  swap (Right x) = Left x

instance Swap (,) where
  swap (a, b) = (b, a)

ex1 :: Either Int String
ex1 = Right "hei"

ex2 :: ([Int], Maybe Bool)
ex2 = ([1, 2, 3], Just True)

-- Klassen skal ha en funksjon 'swap' slik at man kan gjøre om ex1 til en verdi med type Either String Int
-- og ex2 til (Maybe Bool, [Int])

-- Hvordan ser klassen ut? Hva blir kinden?
-- Hva blir typen til swap?
-- implenter instancec for Either og (,)

-- Foldable

-- Lag foldable instance for
data Tree a = Empty | Bin (Tree a) a (Tree a) deriving (Foldable)
instance Monoid Tree where
  mempty = Empty
  mappend a Empty = a
  mappend Empty b = b
  mappend (Bin al av ar) (Bin bl bv br) =
    Bin (al<>bl) (av<>bv) (ar<>br)

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap lift Empty = Empty
  foldMap lift (Bin l a r) =
  -- foldr :: (a -> b -> b) -> b -> t a -> b

exTree = Bin (Bin Empty 1 Empty) 2 (Bin (Bin Empty 3 (Bin Empty 4 Empty)) 5 Empty)

--bruk foldr til å få folde exTree for å få ut følgende verdier :

-- Som en liste (inorder) : [1,2,3,4,5]
-- Som en liste (inorder), men uten oddetall : [2,4]
-- størrelsen : 5
-- Litt vanskeligere :
-- Første elementet : Just 1
-- Partisjonere partall og odetall : ([2,4],[1,3,5])

--skriv følgende funksjon med foldr
toList' :: Foldable f => f a -> [a]
toList' = undefined

-- Monoids

-- skriv en Monoid Instance for
data Three = T1 | T2 | T3

-- "sketsj" noen flere i hodet . Ta inspirasjon fra fra tidligere monoids vi har sett på

-- Bruker fold/foldMap til å kombinere strengere i exFold (altså "abc")
exFold :: [Maybe String]
exFold = [Just "a", Nothing, Just "b", Just "c"]

-- Dual kan brukes når man vil ha den "flippa" monoiden
-- bruk Dual og foldMap til å reversere en liste (du kan bruke getDual til å pakke ut, men ikke nødvendig)
newtype Dual a = Dual {getDual :: a} deriving (Show)

instance Semigroup a => Semigroup (Dual a) where
    Dual a <> Dual b = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty
