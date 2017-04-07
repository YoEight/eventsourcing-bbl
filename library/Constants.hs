--------------------------------------------------------------------------------
-- |
-- Module : Constants
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Constants where

--------------------------------------------------------------------------------
import ClassyPrelude

--------------------------------------------------------------------------------
-- | Horizontal slot number.
horizontalSlotNum :: Int
horizontalSlotNum = 7

--------------------------------------------------------------------------------
-- | Vertical slot number.
verticalSlotNum :: Int
verticalSlotNum = 6

--------------------------------------------------------------------------------
-- | The width of a slot.
slotWidth :: Int
slotWidth = 10

--------------------------------------------------------------------------------
-- | The height of a slot.
slotHeight :: Int
slotHeight = 10 

--------------------------------------------------------------------------------
-- | The width of the game board.
boardWidth :: Int
boardWidth = horizontalSlotNum * slotWidth

--------------------------------------------------------------------------------
-- | The height of the game board.
boardHeight :: Int
boardHeight = verticalSlotNum * slotHeight

--------------------------------------------------------------------------------
margin :: Int
margin = 2

--------------------------------------------------------------------------------
originX :: Int
originX = 10

--------------------------------------------------------------------------------
originY :: Int
originY = 2

--------------------------------------------------------------------------------
-- | The total slot number of the game board.
slotNums :: Int
slotNums = horizontalSlotNum * verticalSlotNum