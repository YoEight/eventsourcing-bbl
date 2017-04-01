{-# LANGUAGE OverloadedLists #-}
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
import Control.Lens hiding (cons)
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty)
import Graphics.Vty

--------------------------------------------------------------------------------
type ColumnIndex = Int

--------------------------------------------------------------------------------
data Token = Circle | Cross deriving Show

--------------------------------------------------------------------------------
data GameEvent
  = Start
  | KeyPressed Key [Modifier]

--------------------------------------------------------------------------------
data Player = Player1 | Player2

--------------------------------------------------------------------------------
data Phase
  = Init
  | Gaming

--------------------------------------------------------------------------------
data Board =
  Board { _piles :: HashMap ColumnIndex (NonEmpty Token) }

--------------------------------------------------------------------------------
makeLenses ''Board

--------------------------------------------------------------------------------
data GameState =
  GameState { _board     :: Board
            , _phase     :: Phase
            , _cursorPos :: Int
            , _player    :: Player
            }

--------------------------------------------------------------------------------
makeLenses ''GameState

--------------------------------------------------------------------------------
newGameState :: GameState
newGameState =
  GameState { _board     = Board mempty
            , _phase     = Init
            , _cursorPos = 1
            , _player    = Player1
            }

--------------------------------------------------------------------------------
type Game = StateT GameState IO

--------------------------------------------------------------------------------
insertToken :: ColumnIndex -> Token -> Game ()
insertToken idx tok = board.piles %= alterMap go idx
  where
    go :: Maybe (NonEmpty Token) -> Maybe (NonEmpty Token)
    go Nothing     = Just [tok]
    go (Just pile) = Just (cons tok pile)