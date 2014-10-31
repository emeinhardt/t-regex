{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
module Data.Regex.MultiGenerics (
  Regex(Regex),
  empty_, none,
  any_,
  inj,
  square, var,
  choice, (<||>),
  concat_, (<--),
  iter,
  capture,
  matches,
  match,
  Fix(..)
) where

import Control.Applicative
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.MultiGenerics

import Unsafe.Coerce -- :(

-- As defined in page 58 of "Tree Automata Techniques and Applications"
-- * f -> set of constructors
-- * c -> set of possible keys for capturing
-- * s -> set of iteration and concatenation positions
-- * ix -> index over the data types
data Regex' (s :: k -> *) c (f :: (k -> *) -> k -> *) (ix :: k) where
  Empty   :: Regex' s c f ix
  Any     :: Regex' s c f ix
  Inject  :: f (Regex' s c f) ix -> Regex' s c f ix
  Square  :: s ix -> Regex' s c f ix
  Choice  :: Regex' s c f ix -> Regex' s c f ix -> Regex' s c f ix
  Concat  :: (s xi -> Regex' s c f ix) -> Regex' s c f xi -> Regex' s c f ix
  Capture :: c -> Regex' s c f ix -> Regex' s c f ix
newtype Regex c f ix = Regex { unRegex :: forall s. Regex' s c f ix }

empty_ :: Regex' k c f ix
empty_ = Empty

none :: Regex' k c f ix
none = empty_

any_ :: Regex' k c f ix
any_ = Any

inj :: f (Regex' k c f) ix -> Regex' k c f ix
inj = Inject

square :: k ix -> Regex' k c f ix
square = Square

var :: k ix -> Regex' k c f ix
var = square

choice :: Regex' k c f ix -> Regex' k c f ix -> Regex' k c f ix
choice = Choice

(<||>) :: Regex' k c f ix -> Regex' k c f ix -> Regex' k c f ix
(<||>) = choice

concat_ :: (k xi -> Regex' k c f ix) -> Regex' k c f xi -> Regex' k c f ix
concat_ = Concat

(<--) :: (k xi -> Regex' k c f ix) -> Regex' k c f xi -> Regex' k c f ix
(<--) = concat_

iter :: (k ix -> Regex' k c f ix) -> Regex' k c f ix
iter r = Concat r (iter r)

capture :: c -> Regex' k c f ix -> Regex' k c f ix
capture = Capture

matches :: (Generic1m f, MatchG (Rep1m f))
        => Regex c f ix -> Fix f ix -> Bool
r `matches` t = matches' (unRegex r) t 0 []

data Result f where
  Result :: Sing ix -> Fix f ix -> Result f

match :: (Ord c, Generic1m f, MatchG (Rep1m f), Alternative m, SingI ix)
      => Regex c f ix -> Fix f ix -> Maybe (Map c (m (Result f)))
match r t = match' (unRegex r) t 0 []

newtype WrappedInteger a = W Integer

matches' :: (Generic1m f, MatchG (Rep1m f))
         => Regex' WrappedInteger c f ix
         -> Fix f ix
         -> Integer  -- Fresh variable generator
         -> [(Integer, forall xi. Regex' WrappedInteger c f xi)]  -- Ongoing substitution
         -> Bool
matches' Empty            _ _ _ = False
matches' Any              _ _ _ = True
matches' (Inject r) (Fix t) i s = matchesG (from1k r) (from1k t) i s
matches' (Square (W n))   t i s = let Just r = unsafeCoerce (lookup n s) in matches' r t i s
matches' (Choice r1 r2)   t i s = matches' r1 t i s || matches' r2 t i s
matches' (Concat r1 r2)   t i s = matches' (r1 (W i)) t (i+1) ((i, unsafeCoerce r2):s)
matches' (Capture _ r)    t i s = matches' r t i s

match' :: (Ord c, Generic1m f, MatchG (Rep1m f), Alternative m, SingI ix)
       => Regex' WrappedInteger c f ix
       -> Fix f ix
       -> Integer  -- Fresh variable generator
       -> [(Integer, forall xi. Regex' WrappedInteger c f xi)]  -- Ongoing substitution
       -> Maybe (Map c (m (Result f)))
match' Empty            _ _ _ = Nothing
match' Any              _ _ _ = Just M.empty
match' (Inject r) (Fix t) i s = matchG (from1k r) (from1k t) i s
match' (Square (W n))   t i s = let Just r = unsafeCoerce (lookup n s) in match' r t i s
match' (Choice r1 r2)   t i s = match' r1 t i s <|> match' r2 t i s
match' (Concat r1 r2)   t i s = match' (r1 (W i)) t (i+1) ((i, unsafeCoerce r2):s)
match' (Capture c r)    t i s = M.insertWith (<|>) c (pure (Result sing t)) <$> match' r t i s

class MatchG (f :: (k -> *) -> k -> *) where
  matchesG :: (Generic1m g, MatchG (Rep1m g))
           => f (Regex' WrappedInteger c g) ix
           -> f (Fix g) ix
           -> Integer
           -> [(Integer, forall xi. Regex' WrappedInteger c g xi)]
           -> Bool
  matchG :: (Ord c, Generic1m g, MatchG (Rep1m g), Alternative m, SingI ix)
         => f (Regex' WrappedInteger c g) ix
         -> f (Fix g) ix
         -> Integer
         -> [(Integer, forall xi. Regex' WrappedInteger c g xi)]
         -> Maybe (Map c (m (Result g)))

instance MatchG U1m where
  matchesG _ _ _ _ = True
  matchG   _ _ _ _ = Just M.empty

instance SingI xi => MatchG (Par1m xi) where
  matchesG (Par1m r) (Par1m t) = matches' r t
  matchG   (Par1m r) (Par1m t) = match' r t

instance (MatchG f, SingI xi) => MatchG (Rec1m f xi) where
  matchesG (Rec1m r) (Rec1m t) = matchesG r t
  matchG   (Rec1m r) (Rec1m t) = matchG r t

instance Eq c => MatchG (K1m i c) where
  matchesG (K1m r) (K1m t) _ _ = r == t
  matchG   (K1m r) (K1m t) _ _ = do guard (r == t)
                                    return M.empty

instance (MatchG a, MatchG b) => MatchG (a :++: b) where
  matchesG (L1m r) (L1m t) i s = matchesG r t i s
  matchesG (R1m r) (R1m t) i s = matchesG r t i s
  matchesG _       _       _ _ = False
  matchG   (L1m r) (L1m t) i s = matchG r t i s
  matchG   (R1m r) (R1m t) i s = matchG r t i s
  matchG   _       _       _ _ = Nothing

instance (MatchG a, MatchG b) => MatchG (a :**: b) where
  matchesG (r1 :**: r2) (t1 :**: t2) i s = matchesG r1 t1 i s && matchesG r2 t2 i s
  matchG   (r1 :**: r2) (t1 :**: t2) i s = M.unionWith (<|>) <$> matchG r1 t1 i s <*> matchG r2 t2 i s

instance (MatchG f, SingI xi) => MatchG (Tag1m f xi) where
  matchesG (Tag1m r) (Tag1m t) = matchesG r t
  matchG   (Tag1m r) (Tag1m t) = matchG r t