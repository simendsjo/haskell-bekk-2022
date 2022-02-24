{-# LANGUAGE FlexibleInstances #-}
import qualified Data.List (sort)

-- Parametric polymorphishm

--Implementer funksjonene utifra typene :

constant :: a -> b -> a
constant a _ = a

f :: (a -> b -> c) -> (a -> b) -> a -> c
f abc ab a = abc a (ab a)


-- Type classes

-- Eq og Ord

-- 1

-- Implementer Eq og Ord (valgfri, men den må gi ) for disse typene :


data Sjanger = Jazz | Rock | Pop | Klassisk deriving Show 

instance Eq Sjanger where
  (==) Jazz Jazz = True
  (==) Rock Rock = True
  (==) Pop Pop = True
  (==) _ _ = False

instance Ord Sjanger where
  (<=) Jazz _ = True
  (<=) _ Jazz = False
  (<=) Rock _ = True
  (<=) _ Rock = False
  (<=) Pop _ = True
  (<=) _ Pop = False
  (<=) Klassisk _ = True

data NumberedVal a = NumberedVal Int a deriving Show

instance Eq a => Eq (NumberedVal a) where
  (==) (NumberedVal i x) (NumberedVal j y) = i == j && x == y

-- Kan skippe Ord for Tree, for mye jobb
data Tree a b = Empty | NodeA a | NodeB b | Nodes (Tree a b) (Tree a b) 
    deriving Show

instance (Eq a, Eq b) => Eq (Tree a b) where
  (==) Empty Empty = True
  (==) (NodeA x) (NodeA y) = x == y
  (==) (NodeB x) (NodeB y) = x == y
  (==) (Nodes x1 y1) (Nodes x2 y2) = x1 == x2 && y1 == y2
  (==) _ _ = False


-- 2
-- data Maybe a = Nothing | Just a

-- Implementer en funksjon som gir indexen elementet finnes, eller Nothing hvis det ikke finnes.
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ []  = Nothing
findIndex n xs  = go 0 xs
    where
      go i [] = Just i
      go i (x:xs) = if x == n then Just i else go (i+1) xs


-- 3

-- Implementer en funksjon `findMax` som gir det største elementet i en liste.
-- Hva skal funksjonen ha som type?
-- Hva er den mest generelle typen?

findMax :: Ord a => [a] -> Maybe a
findMax [] = Nothing
findMax (x:xs) = go x xs
    where
      go m [] = Just m
      go m (x:xs) = go (max m x) xs

-- 4
-- Egen type class

-- Sortable er en type class for typer der man kan sortere innholdet 
-- den har funksjonen sort
-- sort "1432" ==> "1234"
-- sort (3,1) ==> (1,3)



class Sortable a where
    sort :: a -> a

instance Sortable [Char] where
  sort x = Data.List.sort x

instance Sortable [Int] where
  sort x = Data.List.sort x

instance Sortable (Int, Int) where
  sort t@(x, y) = if x <= y then t else (y, x)

instance Sortable (Char, Char, Char) where
  sort (x, y, z) = let [x', y', z'] = Data.List.sort [x, y, z] in (x', y', z')

-- Implementer for
-- String
-- [Int]
-- (Int,Int)
-- (Char,Char,Char)

-- bruk : Data.List.sort , som er importert


-- 5
-- Gjør instancene mer generelle ved å bruke contexts

instance Ord a => Sortable [a] where
  sort xs = Data.List.sort xs

-- 6
-- Legg til en funksjon : sortReverse


-- 7

main = do
    putStrLn ":)"
