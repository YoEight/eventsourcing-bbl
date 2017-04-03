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
import Control.Monad.State.Strict
import Graphics.Vty

--------------------------------------------------------------------------------
import Game
import Types

--------------------------------------------------------------------------------
gameLoop :: IO ()
gameLoop = do
  cfg <- standardIOConfig
  vty <- mkVty cfg

  _ <- execStateT (go vty Start) newGameState
  shutdown vty
  where
    go vty evt = do
      continue <- react evt
      when continue $ do
        images    <- getImages
        nextEvent <- liftIO $ do
          update vty (picForLayers images)
          let innerLoop = do
                tmp <- nextEvent vty
                case tmp of
                  EvKey key mods    -> return $ KeyPressed key mods
                  _                 -> innerLoop

          innerLoop

        go vty nextEvent