{-# language GADTs, KindSignatures, LambdaCase, RecursiveDo, TypeFamilies #-}
{-# language InstanceSigs, ScopedTypeVariables #-}
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

data Two u (f :: * -> *) = Two (u f) (u f)

instance Reactive u => Reactive (Two u) where
  data Change (Two u) where
    Two1 :: Change u -> Change (Two u)
    Two2 :: Change u -> Change (Two u)
    TwoSwap :: Change (Two u)
    TwoId :: Change (Two u)

  applyChange (Two1 f) (Two a b) = Two (applyChange f a) b
  applyChange (Two2 f) (Two a b) = Two a (applyChange f b)
  applyChange TwoSwap (Two a b) = Two b a
  applyChange TwoId a = a

  freeze (Two a b) = Two <$> freeze a <*> freeze b

  holdInitial (Two a b) eChange =
    Two <$>
    holdInitial
      a
      (fmapMaybe
         (\case; Two1 x -> Just x; _ -> Nothing)
         eChange
      ) <*>
    holdInitial
      b
      (fmapMaybe
         (\case; Two2 x -> Just x; _ -> Nothing)
         eChange
      )

  holdReactive z eChange = do
    rec
      deChange <-
        holdDyn eChange $
        attachWithMaybe
          (\e ->
           \case
             TwoSwap ->
               Just $
               (\case
                  Two1 f -> Two2 f
                  Two2 f -> Two1 f
                  TwoSwap -> TwoId
                  TwoId -> TwoSwap
               ) <$>
               e
             _ -> Nothing
          )
          (current deChange)
          eChange
    let eChange' = switchDyn deChange
    init <- holdInitial z eChange'
    rec
      dyn <-
        holdDyn
          init
          (attachWithMaybe
            (\(Two a b) ->
             \case
                TwoSwap -> Just $ Two b a
                _ -> Nothing
            )
            (current dyn)
            eChange
          )
    pure dyn
