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
import Constants

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
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

--------------------------------------------------------------------------------
playerToken :: Player -> Token
playerToken Player1 = Circle
playerToken Player2 = Cross

--------------------------------------------------------------------------------
data Phase
  = Init
  | Gaming

--------------------------------------------------------------------------------
data Board =
  Board { _piles :: HashMap ColumnIndex (NonEmpty Token) }

--------------------------------------------------------------------------------
boardTokens :: Board -> [(ColumnIndex, NonEmpty Token)]
boardTokens = mapToList . _piles

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
insertToken :: ColumnIndex -> Token -> Game Bool
insertToken idx tok = do
  m <- use (board.piles)
  case lookup idx m of
    Just pile
      | length pile == verticalSlotNum -> return False
      | otherwise -> do
        board.piles .= insertMap idx (cons tok pile) m
        return True
    Nothing -> do
      board.piles .= insertMap idx [tok] m
      return True