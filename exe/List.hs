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
import Data.Reactive.List (List(..), Change(..))
import qualified Data.Reactive.List as List
import Data.Reactive.Leaf (Leaf(..), Change(..))
import qualified Data.Reactive.Leaf as Leaf

type ReactiveString = List (Leaf Char)

toString :: ReactiveString Identity -> String
toString Nil = []
toString (Cons (Leaf (Identity a)) (Identity b)) =
  a : toString  b

data Command
  = InsertAt Int Char
  | ReplaceAt Int Char
  | DeleteAt Int
  | Quit
  deriving Read

commandToChange :: Command -> Maybe (Change ReactiveString)
commandToChange (ReplaceAt 0 c) =
  Just $
  Cons1 . LeafAll $ const (Leaf $ Identity c)
commandToChange (ReplaceAt n c) =
  Cons2 <$> commandToChange (ReplaceAt (n-1) c)
commandToChange (InsertAt 0 c) =
  Just $
  ListAll (Cons (Leaf $ Identity c) . Identity)
commandToChange (InsertAt n c) =
  Cons2 <$> commandToChange (InsertAt (n-1) c)
commandToChange (DeleteAt 0) =
  Just $
  ListAll (\case; Nil -> Nil; Cons _ (Identity a) -> a)
commandToChange (DeleteAt n) =
  Cons2 <$> commandToChange (DeleteAt (n-1))
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
    (dString, eQuit) <- network eCommand

    let
      printUpdated ps d =
        performEvent_ $
          liftIO . putStrLn . (ps <>) . (": " <>) . toString <$>
          updated (d >>= freeze)

    printUpdated "normal" dString
    printUpdated "succ'd" $ List.map (Leaf.map succ) <$> dString

    pure eQuit

network ::
  (Reflex t, MonadHold t m, Adjustable t m, MonadFix m) =>
  Event t Command ->
  m
    ( Dynamic t (ReactiveString (Dynamic t))
    , Event t ()
    )
network eCommand = do
  dString <- holdReactive Nil (fmapMaybe commandToChange eCommand)
  pure
    ( dString
    , fmapMaybe
        (\case; Quit -> Just (); _ -> Nothing)
        eCommand
    )
