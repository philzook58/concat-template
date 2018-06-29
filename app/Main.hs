{-#LANGUAGE GADTs, KindSignatures, TypeFamilies #-}
module Main where

import Lib
import ConCat.AltCat
import qualified ConCat.Category
import ConCat.Rebox 
import Prelude hiding (id,(.),curry,uncurry,const,unzip)


data FreeCat a b where
   Id :: FreeCat a a
   Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
   Par :: FreeCat a b -> FreeCat c d -> FreeCat (a,c) (b,d)
   Exl :: FreeCat (a,b) a
   Exr :: FreeCat (a,b) b
   Dup :: FreeCat a (a,a)

data FreeCat' a b = Id' deriving Show

instance Show (FreeCat a b) where
   show Id = "Id"
   show (Comp f g) = "(Comp" ++ (show f) ++ " " ++ (show g) ++ ")"
   show _ = ""

instance Category (FreeCat) where
  -- type Ok (FreeCat) = ()
  id  = Id
  (.) = Comp

instance Category (FreeCat') where
  -- type Ok (FreeCat) = ()
  id  = Id'
  (.) = undefined


instance MonoidalPCat (FreeCat) where
  (***) = Par -- or default

instance AssociativePCat (FreeCat)
instance BraidedPCat     (FreeCat)

instance ProductCat (FreeCat) where
  -- type Prod (:>) = (:*)
  exl = Exl
  exr = Exr
  dup = Dup

--instance UnitCat (FreeCat)

-- instance OpCon (:+) (Sat GenBuses) where inOp = Entail (Sub Dict)

-- instance CoproductCat (:>) where
--   inl = namedC "inl"
--   inr = namedC "inr"
--   f ||| g = namedC "|||"
{-
instance CoproductPCat (:>) where
  inlP   = namedC "inlP"
  inrP   = namedC "inrP"
jamP = namedC "jamP"
-}

freecat :: FreeCat a b -> FreeCat a b
freecat = id

main :: IO ()
main = do
	putStrLn $ show $ (toCcc' (\x y -> (y, x))) "larry" "fred" -- Conversion of lambda to itself. Seems to work
	putStrLn $ show $ freecat $ ((toCcc' (\x -> x))) -- Does not work
