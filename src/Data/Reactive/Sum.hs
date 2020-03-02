{-# language GADTs, KindSignatures, LambdaCase, RecursiveDo, TypeFamilies #-}
module Data.Reactive.Sum where

import Data.Functor.Identity
import Data.Reactive.Class
import Reflex
import Reflex.Network

data Sum u v (f :: * -> *)
  = L (u f)
  | R (v f)

instance (Reactive u, Reactive v) => Reactive (Sum u v) where
  data Change (Sum u v) where
    SumL1 :: Change u -> Change (Sum u v)
    SumR1 :: Change v -> Change (Sum u v)
    SumAll :: (Sum u v Identity -> Sum u v Identity) -> Change (Sum u v)

  applyChange (SumL1 f) (L a) = L (applyChange f a)
  applyChange SumL1{} a = a
  applyChange (SumR1 f) (R a) = R (applyChange f a)
  applyChange SumR1{} a = a
  applyChange (SumAll f) x = f x

  freeze (L a) = L <$> freeze a
  freeze (R a) = R <$> freeze a

  holdInitial (L a) eChange =
    L <$>
    holdInitial a (fmapMaybe (\case; SumL1 x -> Just x; _ -> Nothing) eChange)
  holdInitial (R a) eChange =
    R <$>
    holdInitial a (fmapMaybe (\case; SumR1 x -> Just x; _ -> Nothing) eChange)

  holdReactive z eChange = do
    rec
      dyn <-
        networkHold
          (holdInitial z eChange)
          (attachWithMaybe
            (\val ->
             \case
                SumAll f ->
                  Just $
                  flip holdInitial eChange . f =<<
                  sample (current $ freeze val)
                _ -> Nothing
            )
            (current dyn)
            eChange
          )
    pure dyn
