{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module : Types
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Types where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Lens
import Control.Monad.State.Strict

--------------------------------------------------------------------------------
data GameState =
  GameState { _posX :: Int
            , _posY :: Int
            }

--------------------------------------------------------------------------------
makeLenses ''GameState

--------------------------------------------------------------------------------
type Game = StateT GameState IO
