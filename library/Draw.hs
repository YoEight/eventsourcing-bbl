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
generate :: (Int -> Maybe Char) -> String
generate k = go 1
  where
    go i =
      case k i of
        Just c  -> c : go (i + 1)
        Nothing -> []

--------------------------------------------------------------------------------
placeSlotAt :: Int -> Int -> Slot -> Image
placeSlotAt slotX slotY s = do
  translate posX posY (slot s)
  where
    posX  = originX + (slotX - 1) * slotWidth + 1
    posY  = originY + normY * slotHeight + 1
    normY = verticalSlotNum - slotY

--------------------------------------------------------------------------------
boardImage :: Image
boardImage = foldMap go [1..(boardHeight + 1)]
  where
    go 1 = plainLine
    go line
      | (line - 1) `mod` slotHeight == 0 = plainLine
      | line > boardHeight               = mempty
      | otherwise                        = boardLine

--------------------------------------------------------------------------------
boardVerticalBorder :: Image
boardVerticalBorder = string defAttr (replicate (boardWidth + 1) '=')

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
placePlayerCursorAt :: Player -> Int -> Image
placePlayerCursorAt p pos =
  translateX posX (playerCursor p)
  where
    posX  = originX + (pos - 1) * slotWidth + 1

--------------------------------------------------------------------------------
playerCursor :: Player -> Image
playerCursor p = string attr (replicate (slotWidth -1) '=')
  where
    attr  = defAttr `withForeColor` color
    color =
      case p of
        Player1 -> red
        Player2 -> yellow

--------------------------------------------------------------------------------
placeTextAt :: Pos -> String -> Image
placeTextAt (x,y) s =
  translate (originX + x) (originY + y) (string defAttr s)

--------------------------------------------------------------------------------
drawSlots :: Board -> [Image]
drawSlots b = fmap drawing boardPositions
  where
    drawing pos@(x,y) = placeSlotAt x y (boardGetSlot b pos)

--------------------------------------------------------------------------------
-- // Drawing
--------------------------------------------------------------------------------
block :: Char
block = chr 9608