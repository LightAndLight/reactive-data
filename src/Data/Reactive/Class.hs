{-# language KindSignatures, TypeFamilies #-}
module Data.Reactive.Class where

import Control.Monad.Fix
import Data.Functor.Identity
import Reflex


class Reactive (v :: (* -> *) -> *) where
  data Change v :: *
  applyChange :: Change v -> (v Identity -> v Identity)
  freeze :: Monad f => v f -> f (v Identity)
  holdInitial ::
    (Reflex t, MonadHold t m, Adjustable t m, MonadFix m) =>
    v Identity ->
    Event t (Change v) ->
    m (v (Dynamic t))
  holdReactive ::
    (Reflex t, MonadHold t m, Adjustable t m, MonadFix m) =>
    v Identity ->
    Event t (Change v) ->
    m (Dynamic t (v (Dynamic t)))
