{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Concurrent
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Reflex
import Reflex.Host.Basic

import Data.Reactive.Class
import Data.Reactive.Pair (Two(..), Change(..))
import qualified Data.Reactive.List as List
import Data.Reactive.Leaf (Leaf(..), Change(..))
import qualified Data.Reactive.Leaf as Leaf

type ReactivePair = Two (Leaf Int)

toString :: ReactivePair Identity -> String
toString (Two (Leaf (Identity a)) (Leaf (Identity b))) =
  show (a, b)

data Command
  = Fst Int
  | Snd Int
  | Swap
  | Quit
  deriving Read

commandToChange :: Command -> Maybe (Change ReactivePair)
commandToChange (Fst a) =
  Just $
  Two1 . LeafAll $ const (Leaf $ Identity a)
commandToChange (Snd a) =
  Just $
  Two2 . LeafAll $ const (Leaf $ Identity a)
commandToChange Swap = Just TwoSwap
commandToChange Quit = Nothing

main :: IO ()
main = do
  basicHostWithQuit $ do
    (eCommand, triggerCommand) <- newTriggerEvent
    let
      loop = do
        l <- getLine
        let cmd = read l :: Command
        triggerCommand cmd
        loop
    liftIO $ forkIO loop
    (dPair, eQuit) <- network eCommand

    performEvent_ $
      liftIO . putStrLn . toString <$>
      updated (dPair >>= freeze)

    pure eQuit

network ::
  (Reflex t, MonadHold t m, Adjustable t m, MonadFix m) =>
  Event t Command ->
  m
    ( Dynamic t (ReactivePair (Dynamic t))
    , Event t ()
    )
network eCommand = do
  dPair <- holdReactive (Two (Leaf (Identity 0)) (Leaf (Identity 0))) (fmapMaybe commandToChange eCommand)
  pure
    ( dPair
    , fmapMaybe
        (\case; Quit -> Just (); _ -> Nothing)
        eCommand
    )
