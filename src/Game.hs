module Game where

import qualified System.Random as Random
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as MMaybe

import qualified Battleship
import qualified Sea
import qualified Setup
import qualified Control.Monad

{-# ANN module "HLint: ignore Use camelCase" #-}

type Pos      = Sea.Pos
data Result   = Victory | Defeat | Tie | Yield | EnemyYield deriving(Read, Show, Eq)
type ServerResponse = (Pos, Maybe Result)
