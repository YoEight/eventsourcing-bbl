--------------------------------------------------------------------------------
-- |
-- Module : Lib
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lib
  ( gameLoop
  , runGame
  ) where

--------------------------------------------------------------------------------
import ClassyPrelude hiding (replicateM)
import Control.Lens
import Control.Monad.State.Strict

--------------------------------------------------------------------------------
import Lifted
import Types

--------------------------------------------------------------------------------
initGameState :: GameState
initGameState =
  GameState { _posX = 0
            , _posY = 0 
            }

--------------------------------------------------------------------------------
runGame :: Game () -> IO ()
runGame action = evalStateT action initGameState

--------------------------------------------------------------------------------
gameLoop :: Game ()
gameLoop = do
  drawBoard

--------------------------------------------------------------------------------
drawBoard :: Game ()
drawBoard = do
  clearScreen

  let boardWidth = horizontalSlotNum * slotWidth

  for_ [1..verticalSlotNum] $ \line -> do
    setCursorColumn margin

    printChar '|'
    replicateM_ (boardWidth - 1) $ do
      let char = if line == 1 then '=' else '-'
      printChar char
    printChar '|'

    replicateM_ (slotHeight - 2) $ do
       cursorDown 1
       setCursorColumn margin
       printChar '|'

       cursorBackward 1
       for_ [1..(verticalSlotNum + 1)] $ \col -> do
          setCursorColumn (margin + (col * slotWidth))
          printChar '|'

  setCursorColumn margin
  replicateM_ boardWidth $
    printChar '='
  printChar '|'

--------------------------------------------------------------------------------
-- // Game constants
--------------------------------------------------------------------------------
horizontalSlotNum :: Int
horizontalSlotNum = 7

--------------------------------------------------------------------------------
verticalSlotNum :: Int
verticalSlotNum = 6

--------------------------------------------------------------------------------
slotWidth :: Int
slotWidth = 5

--------------------------------------------------------------------------------
slotHeight :: Int
slotHeight = 5 

--------------------------------------------------------------------------------
margin :: Int
margin = 2

--------------------------------------------------------------------------------
-- \\
--------------------------------------------------------------------------------
