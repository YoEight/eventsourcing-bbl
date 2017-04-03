--------------------------------------------------------------------------------
-- |
-- Module : Game
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Game where

--------------------------------------------------------------------------------
import ClassyPrelude
import Control.Lens hiding (snoc)
import Graphics.Vty

--------------------------------------------------------------------------------
import Constants
import Draw
import Types

--------------------------------------------------------------------------------
react :: GameEvent -> Game Bool
react Start = do
  phase .= Gaming

  return True
react (KeyPressed key mods) = do
  p <- use phase
  case p of
    Gaming -> handleGamingPressed key mods
    _      -> return True

--------------------------------------------------------------------------------
getImages :: Game [Image]
getImages = do
  p <- use phase
  case p of
    Init -> return []
    Gaming -> do
      pos <- use cursorPos
      ply <- use player
      b   <- use board

      let playerCur = placePlayerCursorAt ply pos
          playboard = translate originX originY  boardImage
          landscape = playerCur : snoc (drawSlots b) playboard

      return landscape

--------------------------------------------------------------------------------
handleGamingPressed :: Key -> [Modifier] -> Game Bool
handleGamingPressed key mods =
  case key of
    KLeft -> do
      pos <- use cursorPos
      when (pos - 1 >= 1) $ do
        cursorPos -= 1

      return True

    KRight -> do
      pos <- use cursorPos
      when (pos + 1 <= 7) $ do
        cursorPos += 1

      return True

    KChar 'c' ->
      case mods of
        [MCtrl] -> return False
        _       -> return True

    KEnter -> do
      pos     <- use cursorPos
      succeed <- insertToken pos
      p       <- use player

      when succeed $ do
        player %= nextPlayer

      if succeed
        then do
          outcome <- checkWin
          return (outcome /= Just p)
        else return True

    _ -> return True
