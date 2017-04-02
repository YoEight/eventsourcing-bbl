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
placeSlotAt :: Int -> Int -> Token -> Image
placeSlotAt slotX slotY token = do
  translate posX posY (slot token)
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
slot :: Token -> Image
slot token = foldMap (\_ -> blockLine) [1..(slotHeight - 1)]
  where
    blockLine = string attr (replicate (slotWidth - 1) block)
    attr      = defAttr `withForeColor` color
    color     =
      case token of
        Circle -> red
        Cross  -> yellow

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
drawTokens :: Board -> [Image]
drawTokens = foldMap drawing . boardTokens
  where
    drawing (col, pile) = go col 1 (reverse $ toList pile)

    go col i (t:ts) = placeSlotAt col i t : go col (i+1) ts
    go _ _ []       = []

--------------------------------------------------------------------------------
-- // Drawing
--------------------------------------------------------------------------------
block :: Char
block = chr 9608