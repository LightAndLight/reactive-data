{-# language GADTs, KindSignatures, LambdaCase, RecursiveDo, TypeFamilies #-}
module Data.Reactive.Pair where

import Data.Functor.Identity
import Data.Reactive.Class
import Reflex
import Reflex.Network

data Pair u v (f :: * -> *) = Pair (u f) (v f)

instance (Reactive u, Reactive v) => Reactive (Pair u v) where
  data Change (Pair u v) where
    Pair1 :: Change u -> Change (Pair u v)
    Pair2 :: Change v -> Change (Pair u v)
    Pair12 :: Change u -> Change v -> Change (Pair u v)

  applyChange (Pair1 f) (Pair a b) = Pair (applyChange f a) b
  applyChange (Pair2 f) (Pair a b) = Pair a (applyChange f b)
  applyChange (Pair12 f g) (Pair a b) = Pair (applyChange f a) (applyChange g b)

  freeze (Pair a b) = Pair <$> freeze a <*> freeze b

  holdInitial (Pair a b) eChange =
    Pair <$>
    holdInitial
      a
      (fmapMaybe
         (\case
             Pair1 x -> Just x
             Pair12 x _ -> Just x
             _ -> Nothing
         )
         eChange
      ) <*>
    holdInitial
      b
      (fmapMaybe
         (\case
             Pair2 x -> Just x
             Pair12 _ x -> Just x
             _ -> Nothing
         )
         eChange
      )

  holdReactive z eChange = do
    rec
      dyn <-
        networkHold
          (holdInitial z eChange)
          never
    pure dyn
