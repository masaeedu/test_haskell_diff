{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

import GHC.Exts

type family Each (f :: k -> Constraint) (cs :: [k]) :: Constraint where
  Each _ '[] = ()
  Each f (c ': cs) = (f c, Each f cs)

data HList (xs :: [*]) where
  HNil :: HList '[]
  (:::) :: x -> HList xs -> HList (x ': xs)

infixr 1 :::

instance Each Eq xs => Eq (HList xs) where
  HNil == HNil = True 
  (x ::: xs) == (y ::: ys) = x == y && xs == ys

main = print $ (1 ::: "foo" ::: HNil) == (2 ::: "bar" ::: HNil)
