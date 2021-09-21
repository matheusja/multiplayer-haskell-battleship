module Display where

import qualified System.IO as I_O
import qualified System.Console.ANSI as Console

import Data.Bifunctor(first)

type Pos = (Int, Int)

resetScreen = do
  Console.clearScreen
  Console.setCursorPosition 0 0
  
putStrRed text = do
  Console.setSGR [Console.SetColor Console.Foreground Console.Vivid Console.Red]
  I_O.putStr text
  Console.setSGR [Console.Reset]
  
-- Display.withCursor
withCursor :: Pos -> [[Char]] -> String
withCursor pc = unlines . fmap showLine . zip [0..]
  where
    showLine :: (Int, [Char]) -> String
    showLine (y, l) = concat . fmap (showChar . (first (flip (,) y))) $ zip [0..] l
    
    showChar :: (Pos,Char) -> String
    showChar (p, c) =
      let (l, r) = if p == pc then ('[', ']') else (' ', ' ') in l:c:r:[]

withoutCursor :: [[Char]] -> String
withoutCursor = unlines . fmap concat . (fmap . fmap) ((' ':) . (:' ':[]))


