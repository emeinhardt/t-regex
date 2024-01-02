module Test.QuickCheck.Arbitrary1 (
  Arbitrary1(..)
) where

import Control.Applicative
import Test.QuickCheck

-- | Version of 'Arbitrary' for functors.
class Arbitrary1 f where
  liftArbitrary :: Gen a -> Gen (f a)

-- From QuickCheck source
instance Arbitrary1 [] where
  liftArbitrary g = sized $ \n ->
    do k <- choose (0,n)
       sequence [ g | _ <- [1..k] ]

instance Arbitrary1 Maybe where
  liftArbitrary g = frequency [(1, return Nothing), (3, Just <$> g)]
