{-# LANGUAGE DeriveGeneric   #-}
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
-- This module declares all the types used in the game.
--------------------------------------------------------------------------------
module Types where

--------------------------------------------------------------------------------
import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Vector as Vector
import           EventSource
import           Graphics.Vty

--------------------------------------------------------------------------------
import Constants

--------------------------------------------------------------------------------
type ColumnIndex = Int

--------------------------------------------------------------------------------
data Slot
  = SlotEmpty
  | SlotPlayer1
  | SlotPlayer2
  deriving Eq

--------------------------------------------------------------------------------
data GameEvent
  = Start
    -- ^ Start is used for the first loop of the game. It allows to properly
    --   setting up a game.
  | KeyPressed Key [Modifier]
    -- ^ When the players use their keyboard.


--------------------------------------------------------------------------------
data Player = Player1 | Player2 deriving (Eq, Generic)

--------------------------------------------------------------------------------
instance ToJSON Player
instance FromJSON Player

--------------------------------------------------------------------------------
instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

--------------------------------------------------------------------------------
nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

--------------------------------------------------------------------------------
playerSlot :: Player -> Slot
playerSlot Player1 = SlotPlayer1
playerSlot Player2 = SlotPlayer2

--------------------------------------------------------------------------------
slotPlayer :: Slot -> Maybe Player
slotPlayer SlotPlayer1 = Just Player1
slotPlayer SlotPlayer2 = Just Player2
slotPlayer _           = Nothing

--------------------------------------------------------------------------------
data Phase
  = Init
  | Loading
    -- ^ The players ask to load a game from the database.
  | TimeTravel
    -- ^ The players ask to get back at a given point of the game.
  | Gaming
    -- ^ The players are currently gaming.
  | GameComplete
    -- ^ One of the players has won.

--------------------------------------------------------------------------------
-- A position, (abscissa, ordinate) not 0-based.
type Pos = (Int, Int)

--------------------------------------------------------------------------------
-- A game board.
type Board = Vector.Vector Slot

--------------------------------------------------------------------------------
-- The entire game state.
data GameState =
  GameState { _board     :: Board
            , _phase     :: Phase
            , _cursorPos :: Int
            , _player    :: Player
            , _winner    :: Maybe Player
            , _gameCount :: Int
            , _curGame   :: StreamName
            , _games     :: [StreamName]
            , _moves     :: Int
            }

--------------------------------------------------------------------------------
-- | This generates all the lenses of `GameState`. The lenses can be accessed
--   using any 'GameState' record field stripped from their '_'.
makeLenses ''GameState

--------------------------------------------------------------------------------
-- | Returns the 'Slot' at given position.
boardGetSlot :: Board -> Pos -> Slot
boardGetSlot b pos = b Vector.! (fromCartesian pos - 1)

--------------------------------------------------------------------------------
-- | Converts a position to a flat 'Vector' index.
fromCartesian :: Pos -> Int
fromCartesian (x, y) = x + horizontalSlotNum * (y - 1)

--------------------------------------------------------------------------------
-- | Converts a flat 'Vector' index to cartesian position.
toCartesian :: Int -> (Int, Int)
toCartesian total = go 1 total
  where
    go y agg =
      let remain = agg - horizontalSlotNum in
      if remain >= 1
      then go (y + 1) remain
      else (remain, y)

--------------------------------------------------------------------------------
emptyBoard :: Vector.Vector Slot
emptyBoard = Vector.replicate slotNums SlotEmpty

--------------------------------------------------------------------------------
-- | Returns a 'GameState' initialized with default values.
newGameState :: GameState
newGameState =
  GameState { _board     = emptyBoard
            , _phase     = Init
            , _cursorPos = 1
            , _player    = Player1
            , _winner    = Nothing
            , _gameCount = 0 
            , _curGame   = ""
            , _games     = []
            , _moves     = 0
            }

--------------------------------------------------------------------------------
type Game = ReaderT SomeStore (StateT GameState IO)

--------------------------------------------------------------------------------
-- | Returns the database server connection.
getStore :: Game SomeStore
getStore = ask

--------------------------------------------------------------------------------
-- | Runs a 'Game' computation.
runGame :: Store store => store -> Game () -> IO ()
runGame store game =
  evalStateT (runReaderT game (SomeStore store)) newGameState

--------------------------------------------------------------------------------
-- | Returns all the positions for a given column.
columnIndexes :: Int -> [Pos]
columnIndexes x = [ (x,y) | y <- [1..verticalSlotNum]]

--------------------------------------------------------------------------------
-- | Returns all possible positions of the gaming board.
boardPositions :: [Pos]
boardPositions =
  [ (x,y) | x <- [1..horizontalSlotNum]
          , y <- [1..verticalSlotNum]
          ]

--------------------------------------------------------------------------------
-- | Try to insert a 'Slot' at the given column. If it fails, it returns 'False',
--   Which means this movement isn't allowed.
insertToken :: ColumnIndex -> Game Bool
insertToken idx = do
  v <- use board
  p <- use player

  let loop []         = return False
      loop (pos:rest) =
        let loc = fromCartesian pos - 1 in
        case boardGetSlot v pos of
          SlotEmpty -> do
            board .= (v Vector.// [(loc, playerSlot p)])
            return True
          _ -> loop rest

  loop (columnIndexes idx)

--------------------------------------------------------------------------------
-- | Detects if the current game board has a winner.
checkWin :: Game (Maybe Player)
checkWin = go boardPositions
  where
    go []               = return Nothing
    go ((x,y):rest) = do
      b <- use board

      let slot = boardGetSlot b (x,y)

          onRight =
            x + 3 <= horizontalSlotNum      &&
            boardGetSlot b (x+1, y) == slot &&
            boardGetSlot b (x+2, y) == slot &&
            boardGetSlot b (x+3, y) == slot

          onTop =
            boardGetSlot b (x, y+1) == slot &&
            boardGetSlot b (x, y+2) == slot &&
            boardGetSlot b (x, y+3) == slot

          onUpRight =
            x + 3 <= horizontalSlotNum        &&
            boardGetSlot b (x+1, y+1) == slot &&
            boardGetSlot b (x+2, y+2) == slot &&
            boardGetSlot b (x+3, y+3) == slot

          onUpLeft =
            x - 3 >= 1                        &&
            boardGetSlot b (x-1, y+1) == slot &&
            boardGetSlot b (x-2, y+2) == slot &&
            boardGetSlot b (x-3, y+3) == slot

          won =
            onRight
            || (y + 3 <= verticalSlotNum && (onTop || onUpRight || onUpLeft))

      if slot /= SlotEmpty && won
      then return $ slotPlayer slot
      else go rest

