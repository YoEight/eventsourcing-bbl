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
import Data.List (replicate)

--------------------------------------------------------------------------------
import ClassyPrelude hiding (replicate)
import Control.Lens
import Control.Monad.State.Strict
import Graphics.Vty

--------------------------------------------------------------------------------
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
gameLoop :: IO ()
gameLoop = do
  cfg <- standardIOConfig
  vty <- mkVty cfg

  vtyBoard vty
  e <- nextEvent vty
  shutdown vty
  print e

--------------------------------------------------------------------------------
generate :: (Int -> Maybe Char) -> String
generate k = go 1
  where
    go i =
      case k i of
        Just c  -> c : go (i + 1)
        Nothing -> []

--------------------------------------------------------------------------------
vtyBoard :: Vty -> IO ()
vtyBoard vty = do
  let baseX     = 10
      baseY     = 2
      landscape = [ translate (baseX + 1) (baseY + (slotHeight * 5)) slot
                  , translate baseX baseY boardImage
                  ]

  update vty (picForLayers landscape)

--------------------------------------------------------------------------------
boardImage :: Image
boardImage = foldMap go [1..boardHeight]
  where
    go 1 = boardVerticalBorder
    go line
      | line /= boardHeight && line `mod` slotHeight == 0 = plainLine
      | line == boardHeight = boardVerticalBorder
      | otherwise = boardLine

--------------------------------------------------------------------------------
boardVerticalBorder :: Image
boardVerticalBorder = string defAttr (replicate boardWidth '=')

--------------------------------------------------------------------------------
plainLine :: Image
plainLine = string defAttr (replicate boardWidth '-')

--------------------------------------------------------------------------------
boardLine :: Image
boardLine = string defAttr line
  where
    line = generate $ \col ->
      case col of
        1 -> Just '|'
        _ | col `mod` slotWidth == 0 -> Just '|'
          | col > boardWidth         -> Nothing
          | otherwise                -> Just ' '

--------------------------------------------------------------------------------
slot :: Image
slot = foldMap (\_ -> blockLine) [1..(slotHeight - 1)]
  where
    blockLine = string defAttr (replicate (slotWidth - 2) block)

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
boardHeight :: Int
boardHeight = verticalSlotNum * slotHeight

--------------------------------------------------------------------------------
margin :: Int
margin = 2

--------------------------------------------------------------------------------
-- // Drawing
--------------------------------------------------------------------------------
block :: Char
block = chr 9608