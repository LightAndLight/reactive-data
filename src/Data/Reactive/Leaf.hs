{-# language GADTs, KindSignatures, LambdaCase, RecursiveDo, TypeFamilies #-}
module Data.Reactive.Leaf where

import Data.Functor.Identity
import Data.Reactive.Class
import Reflex
import Reflex.Network

newtype Leaf a (f :: * -> *)
  = Leaf { unLeaf :: f a }

instance Reactive (Leaf a) where
  data Change (Leaf a) where
    LeafAll :: (Leaf a Identity -> Leaf a Identity) -> Change (Leaf a)

  applyChange (LeafAll f) x = f x
  freeze (Leaf a) = Leaf . Identity <$> a
  holdInitial (Leaf (Identity a)) eChange =
    Leaf <$> foldDyn ($) a ((\(LeafAll f) -> runIdentity . unLeaf . f . Leaf . Identity) <$> eChange)
  holdReactive z eChange = constDyn <$> holdInitial z eChange
