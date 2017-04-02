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
slotNums :: Int
slotNums = horizontalSlotNum * verticalSlotNum