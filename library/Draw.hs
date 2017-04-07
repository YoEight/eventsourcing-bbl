--------------------------------------------------------------------------------
-- |
-- Module : Draw
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- This module implement all the drawing related to the game. This is not where
-- a scene is contructed. It defines all the functions needed to construct
-- a scene. Game scenes are built in 'Game' module.
--------------------------------------------------------------------------------
module Draw where

--------------------------------------------------------------------------------
import Data.Char

--------------------------------------------------------------------------------
import ClassyPrelude
import Graphics.Vty

--------------------------------------------------------------------------------
import Constants
import Types

--------------------------------------------------------------------------------
-- | Uses a generator to build a 'String'. The 'Int' passed to the callback is
--   incremented as long as the generation goes. It will stop the first time
--   the callback returns 'Nothing'.
--
--   This a specific implementation of an anamorphism.
generate :: (Int -> Maybe Char) -> String
generate k = go 1
  where
    go i =
      case k i of
        Just c  -> c : go (i + 1)
        Nothing -> []

--------------------------------------------------------------------------------
-- | Draws a 'Slot' at the given position.
placeSlotAt :: Int -> Int -> Slot -> Image
placeSlotAt slotX slotY s = do
  translate posX posY (slot s)
  where
    posX  = originX + (slotX - 1) * slotWidth + 1
    posY  = originY + normY * slotHeight + 1
    normY = verticalSlotNum - slotY

--------------------------------------------------------------------------------
-- | Draws the gaming board.
boardImage :: Image
boardImage = foldMap go [1..(boardHeight + 1)]
  where
    go 1 = plainLine
    go line
      | (line - 1) `mod` slotHeight == 0 = plainLine
      | line > boardHeight               = mempty
      | otherwise                        = boardLine

--------------------------------------------------------------------------------
plainLine :: Image
plainLine = string defAttr (replicate (boardWidth + 1) '-')

--------------------------------------------------------------------------------
boardLine :: Image
boardLine = string defAttr line
  where
    line = generate $ \col ->
      case col of
        1 -> Just '|'
        _ | (col - 1) `mod` slotWidth == 0 -> Just '|'
          | col > boardWidth               -> Nothing
          | otherwise                      -> Just ' '

--------------------------------------------------------------------------------
-- | Draws a 'Slot'.
slot :: Slot -> Image
slot SlotEmpty = mempty
slot s = foldMap (\_ -> blockLine) [1..(slotHeight - 1)]
  where
    blockLine = string attr (replicate (slotWidth - 1) block)
    attr      = defAttr `withForeColor` color
    color     =
      case s of
        SlotPlayer1 -> red
        SlotPlayer2 -> yellow
        _           -> error "impossible in slot."

--------------------------------------------------------------------------------
-- | Draws a 'Player' cursor at the given position.
placePlayerCursorAt :: Player -> Int -> Image
placePlayerCursorAt p pos =
  translateX posX (playerCursor p)
  where
    posX  = originX + (pos - 1) * slotWidth + 1

--------------------------------------------------------------------------------
-- | Draws a 'Player' cursor.
playerCursor :: Player -> Image
playerCursor p = string attr (replicate (slotWidth -1) '=')
  where
    attr  = defAttr `withForeColor` color
    color =
      case p of
        Player1 -> red
        Player2 -> yellow

--------------------------------------------------------------------------------
-- | Displays a message at the given position.
placeTextAt :: Pos -> String -> Image
placeTextAt (x,y) s =
  translate (originX + x) (originY + y) (string defAttr s)

--------------------------------------------------------------------------------
-- | Draws the 'Slot' 's that compase the gaming 'Board'.
drawSlots :: Board -> [Image]
drawSlots b = fmap drawing boardPositions
  where
    drawing pos@(x,y) = placeSlotAt x y (boardGetSlot b pos)

--------------------------------------------------------------------------------
-- | Draws a menu item.
drawMenuItem :: Bool -> String -> Image
drawMenuItem active title = string attr title
  where
    attr =
      if active
      then defAttr `withBackColor` blue
      else defAttr

--------------------------------------------------------------------------------
-- // Drawing
--------------------------------------------------------------------------------
-- | Block unicode value.
block :: Char
block = chr 9608