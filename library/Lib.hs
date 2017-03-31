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
import Data.Char

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
  GameState { _posX  = 0
            , _posY  = 0 
            , _board = Board mempty
            }

--------------------------------------------------------------------------------
runGame :: Game () -> IO ()
runGame action = evalStateT action initGameState

--------------------------------------------------------------------------------
gameLoop :: Game ()
gameLoop = do
  insertToken 0 Circle
  insertToken 0 Circle
  insertToken 0 Circle
  drawBoard

--------------------------------------------------------------------------------
drawBoard :: Game ()
drawBoard = do
  clearScreen

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

  drawTokens
  cursorDown 100

--------------------------------------------------------------------------------
drawTokens :: Game ()
drawTokens = do
  setCursorPosition margin 0
  for_ [0..(horizontalSlotNum - 1)] $ \colIdx -> do
    outcome <- use (board.piles.at colIdx)
    for_ outcome $ \pile -> do
      for_ (zip [1..verticalSlotNum] (toList pile)) $ \(line, token) -> do
        let normalizedLine = verticalSlotNum - line + 1
            colPos  = margin + (colIdx * slotWidth) + 1
            linePos = (normalizedLine - 2) * slotHeight + 1
        setCursorPosition colPos linePos
        drawToken token

--------------------------------------------------------------------------------
drawToken :: Token -> Game ()
drawToken _ = do
  x <- use posX
  replicateM_ (slotHeight - 3) $ do
    replicateM_ (slotWidth - 1) $
      printChar block
    cursorDown 1
    setCursorColumn x

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
slotWidth = 10

--------------------------------------------------------------------------------
slotHeight :: Int
slotHeight = 10 

--------------------------------------------------------------------------------
boardWidth :: Int
boardWidth = horizontalSlotNum * slotWidth

--------------------------------------------------------------------------------
margin :: Int
margin = 2

--------------------------------------------------------------------------------
-- // Drawing
--------------------------------------------------------------------------------
block :: Char
block = chr 9608