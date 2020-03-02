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
    PairAll :: (Pair u v Identity -> Pair u v Identity) -> Change (Pair u v)

  applyChange (Pair1 f) (Pair a b) = Pair (applyChange f a) b
  applyChange (Pair2 f) (Pair a b) = Pair a (applyChange f b)
  applyChange (PairAll f) x = f x

  freeze (Pair a b) = Pair <$> freeze a <*> freeze b

  holdInitial (Pair a b) eChange =
    Pair <$>
    holdInitial a (fmapMaybe (\case; Pair1 x -> Just x; _ -> Nothing) eChange) <*>
    holdInitial b (fmapMaybe (\case; Pair2 x -> Just x; _ -> Nothing) eChange)

  holdReactive z eChange = do
    rec
      dyn <-
        networkHold
          (holdInitial z eChange)
          (attachWithMaybe
            (\val ->
             \case
                PairAll f ->
                  Just $
                  flip holdInitial eChange . f =<<
                  sample (current $ freeze val)
                _ -> Nothing
            )
            (current dyn)
            eChange
          )
    pure dyn
