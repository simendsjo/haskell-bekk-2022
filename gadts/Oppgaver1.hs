{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- skriv om disse datatypene til GADT syntax
data Result e a where
  Err :: e -> Result e a
  Ok :: a -> Result e a

data Info a where
  Good :: {id :: Int, info :: a} -> Info a
  Bad :: {id :: Int, info :: a, errorMsg :: String} -> Info a

--
data IntOrString a where
    MyInt :: Int -> IntOrString Int
    MyString :: String -> IntOrString String

-- implementer funksjonene
incNumber :: IntOrString Int -> IntOrString Int
incNumber (MyInt i) = MyInt (i+1)

reverseString :: IntOrString String -> IntOrString String
reverseString (MyString s)= MyString (reverse s)

setValue :: IntOrString a -> a -> IntOrString a
setValue (MyInt _) x = MyInt x
setValue (MyString _) x = MyString x

-- Exp

data GExp a where
    Var :: String -> GExp Int
    Lit :: Int -> GExp Int
    BoolLit :: Bool -> GExp Bool
    BoolVar :: String -> GExp Bool
    Add :: GExp Int -> GExp Int -> GExp Int
    If :: GExp Bool -> GExp Int -> GExp Int -> GExp Int

-- skriv en eval-funksjon for GExp
eval :: (String -> Int) -> (String -> Bool) -> GExp a -> a
eval i b (Var x)      = i x
eval i b (Lit x)      = x
eval i b (BoolLit x)  = x
eval i b (BoolVar x)  = b x
eval i b (Add x y)    = (eval i b x) + (eval i b y)
eval i b (If p t f)   = if (eval i b p) then (eval i b t) else (eval i b f)

-- skrive en optimize funksjon for GEXp
-- optimaliseringer:
-- If True t f -> t
-- If False t f -> f
-- Add (Lit a) (Lit b) -> Lit (a+b)
-- husk å optimize alle sub-exps
optimize :: GExp a -> GExp a
optimize x@(Var _) = x
optimize x@(Lit _) = x
optimize x@(BoolVar _) = x
optimize x@(BoolLit _) = x
optimize (If (BoolLit True) t _) = optimize t
optimize (If (BoolLit False) _ f) = optimize f
optimize x@(If _ _ _) = x
optimize (Add (Lit a) (Lit b)) = Lit (a + b)
optimize x@(Add _ _) = x


-- hvordan garantere at det at alle nodene er optimized? sånn ish
