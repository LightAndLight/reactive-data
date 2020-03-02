{-# language GADTs, KindSignatures, LambdaCase, RecursiveDo, TypeFamilies #-}
{-# language RankNTypes #-}
module Data.Reactive.List where

import Prelude hiding (map)
import Data.Functor.Identity
import Data.Reactive.Class
import Reflex
import Reflex.Network

import Control.Monad
import Control.Monad.Fix

data List u (f :: * -> *)
  = Nil
  | Cons (u f) (f (List u f))

map :: Functor f => (forall g. Functor g => u g -> v g) -> List u f -> List v f
map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f <$> b)

instance Reactive u => Reactive (List u) where
  data Change (List u) where
    Cons1 :: Change u -> Change (List u)
    Cons2 :: Change (List u) -> Change (List u)

    ListCons :: u Identity -> Change (List u)
    ListUncons :: Change (List u)

  applyChange (Cons1 f) (Cons a b) = Cons (applyChange f a) b
  applyChange Cons1{} a = a
  applyChange (Cons2 f) (Cons a b) = Cons a (applyChange f <$> b)
  applyChange Cons2{} a = a
  applyChange ListUncons (Cons _ (Identity b)) = b
  applyChange (ListCons a) b = Cons a (Identity b)

  freeze Nil = pure Nil
  freeze (Cons a b) = Cons <$> freeze a <*> (fmap Identity . freeze =<< b)

  holdInitial Nil _ = pure Nil
  holdInitial (Cons a (Identity b)) eChange =
    Cons <$>
    holdInitial a (fmapMaybe (\case; Cons1 x -> Just x; _ -> Nothing) eChange) <*>
    holdReactive b (fmapMaybe (\case; Cons2 x -> Just x; _ -> Nothing) eChange)

  holdReactive z eChange = do
    let
      shift match e a =
        case a of
          ListCons{} -> Just $ fmapMaybe (Just . Cons2 <=< match) e
          ListUncons -> Just $ fmapMaybe ((\case; Cons2 a -> Just a; _ -> Nothing) <=< match) e
          Cons2 a' -> shift ((\case; Cons2 x -> Just x; _ -> Nothing) <=< match) e a'
          _ -> Nothing
    rec
      deChange <-
        holdDyn eChange $
        attachWithMaybe
          (shift Just)
          (current deChange)
          eChange
    let eChange' = switchDyn deChange
    rec
      dyn <-
        networkHold
          (holdInitial z eChange')
          (attachWithMaybe
            (\val ->
             \case
                ListCons a ->
                  Just $
                  (\a' -> Cons a' dyn) <$>
                  holdInitial
                    a
                    (fmapMaybe (\case; Cons1 f -> Just f; _ -> Nothing) eChange')
                ListUncons ->
                  case val of
                    Nil -> Nothing
                    Cons _ b -> Just $ sample (current b)
                _ -> Nothing
            )
            (current dyn)
            eChange'
          )
    pure dyn
