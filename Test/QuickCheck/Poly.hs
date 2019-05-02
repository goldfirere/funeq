{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
-- | Types to help with testing polymorphic properties.
--
-- Types 'A', 'B' and 'C' are @newtype@ wrappers around 'Integer' that
-- implement 'Eq', 'Show', 'Arbitrary' and 'CoArbitrary'. Types
-- 'OrdA', 'OrdB' and 'OrdC' also implement 'Ord' and 'Num'.
--
-- See also "Test.QuickCheck.All" for an automatic way of testing
-- polymorphic properties.
module Test.QuickCheck.Poly
  ( A(..), B(..), C(..)
  , OrdA(..), OrdB(..), OrdC(..)
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------
-- polymorphic A, B, C (in Eq)

-- A

newtype A = A{ unA :: Integer }
  deriving ( Eq )

instance Show A where
  showsPrec n (A x) = showsPrec n x

instance Arbitrary A where
  arbitrary    = (A . (+1) . abs) `fmap` arbitrary
  shrink (A x) = [ A x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary A where
  coarbitrary = coarbitrary . unA

-- B

newtype B = B{ unB :: Integer }
  deriving ( Eq )

instance Show B where
  showsPrec n (B x) = showsPrec n x

instance Arbitrary B where
  arbitrary    = (B . (+1) . abs) `fmap` arbitrary
  shrink (B x) = [ B x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary B where
  coarbitrary = coarbitrary . unB

-- C

newtype C = C{ unC :: Integer }
  deriving ( Eq )

instance Show C where
  showsPrec n (C x) = showsPrec n x

instance Arbitrary C where
  arbitrary    = (C . (+1) . abs) `fmap` arbitrary
  shrink (C x) = [ C x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary C where
  coarbitrary = coarbitrary . unC

--------------------------------------------------------------------------
-- polymorphic OrdA, OrdB, OrdC (in Eq, Ord)

-- OrdA

newtype OrdA = OrdA{ unOrdA :: Integer }
  deriving ( Eq, Ord )

liftOrdA
    :: (Integer -> Integer)
    -> OrdA -> OrdA
liftOrdA f (OrdA x) = OrdA (f x)

liftOrdA2
    :: (Integer -> Integer -> Integer)
    -> OrdA -> OrdA -> OrdA
liftOrdA2 f (OrdA x) (OrdA y) = OrdA (f x y)

instance Num OrdA where
    (+)         = liftOrdA2 (+)
    (*)         = liftOrdA2 (*)
    (-)         = liftOrdA2 (-)
    negate      = liftOrdA negate
    abs         = liftOrdA abs
    signum      = liftOrdA signum
    fromInteger = OrdA . fromInteger


instance Show OrdA where
  showsPrec n (OrdA x) = showsPrec n x

instance Arbitrary OrdA where
  arbitrary       = (OrdA . (+1) . abs) `fmap` arbitrary
  shrink (OrdA x) = [ OrdA x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary OrdA where
  coarbitrary = coarbitrary . unOrdA

-- OrdB

newtype OrdB = OrdB{ unOrdB :: Integer }
  deriving ( Eq, Ord )

liftOrdB
    :: (Integer -> Integer)
    -> OrdB -> OrdB
liftOrdB f (OrdB x) = OrdB (f x)

liftOrdB2
    :: (Integer -> Integer -> Integer)
    -> OrdB -> OrdB -> OrdB
liftOrdB2 f (OrdB x) (OrdB y) = OrdB (f x y)

instance Num OrdB where
    (+)         = liftOrdB2 (+)
    (*)         = liftOrdB2 (*)
    (-)         = liftOrdB2 (-)
    negate      = liftOrdB negate
    abs         = liftOrdB abs
    signum      = liftOrdB signum
    fromInteger = OrdB . fromInteger

instance Show OrdB where
  showsPrec n (OrdB x) = showsPrec n x

instance Arbitrary OrdB where
  arbitrary       = (OrdB . (+1) . abs) `fmap` arbitrary
  shrink (OrdB x) = [ OrdB x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary OrdB where
  coarbitrary = coarbitrary . unOrdB

-- OrdC

newtype OrdC = OrdC{ unOrdC :: Integer }
  deriving ( Eq, Ord )

liftOrdC
    :: (Integer -> Integer)
    -> OrdC -> OrdC
liftOrdC f (OrdC x) = OrdC (f x)

liftOrdC2
    :: (Integer -> Integer -> Integer)
    -> OrdC -> OrdC -> OrdC
liftOrdC2 f (OrdC x) (OrdC y) = OrdC (f x y)

instance Num OrdC where
    (+)         = liftOrdC2 (+)
    (*)         = liftOrdC2 (*)
    (-)         = liftOrdC2 (-)
    negate      = liftOrdC negate
    abs         = liftOrdC abs
    signum      = liftOrdC signum
    fromInteger = OrdC . fromInteger

instance Show OrdC where
  showsPrec n (OrdC x) = showsPrec n x

instance Arbitrary OrdC where
  arbitrary       = (OrdC . (+1) . abs) `fmap` arbitrary
  shrink (OrdC x) = [ OrdC x' | x' <- shrink x, x' > 0 ]

instance CoArbitrary OrdC where
  coarbitrary = coarbitrary . unOrdC

--------------------------------------------------------------------------
-- the end.
