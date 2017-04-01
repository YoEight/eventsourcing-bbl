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
  ( gameLoop ) where

--------------------------------------------------------------------------------
import ClassyPrelude
import Graphics.Vty

--------------------------------------------------------------------------------
import Draw
import Types

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
vtyBoard :: Vty -> IO ()
vtyBoard vty = do
  let landscape = [ placeSlotAt 7 6 Circle
                  , translate originX originY boardImage
                  ]

  update vty (picForLayers landscape)
