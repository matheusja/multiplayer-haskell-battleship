module Game where

import qualified Sea

{-# ANN module "HLint: ignore Use camelCase" #-}

type Pos      = Sea.Pos
data Result   = Victory | Defeat | Tie | Yield | EnemyYield | TieYield deriving(Read, Show, Eq)

data ClientTurnDecision = Surrender | Attack Pos deriving(Read, Show, Eq)
data ServerAttackNotify = Miss | Hit | Sunk String deriving(Read, Show, Eq)
data ServerEnemyAttackNotify = JustAttackedAt Pos | JustEnd Result | BothAtackAndEnd Pos Result
  deriving(Read, Show, Eq)
