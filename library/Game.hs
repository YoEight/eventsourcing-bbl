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
import Control.Monad.Except
import Control.Monad.State.Strict
import EventSource
import Graphics.Vty

--------------------------------------------------------------------------------
import Constants
import Event
import Draw
import Types

--------------------------------------------------------------------------------
react :: GameEvent -> Game Bool
react Start = do
  phase .= Init

  return True
react (KeyPressed key mods) = do
  p <- use phase
  case p of
    Init         -> handleMenu key mods
    Loading      -> handleLoad key mods
    Gaming       -> handleGamingPressed key mods
    GameComplete -> handleComplete key mods

--------------------------------------------------------------------------------
getImages :: Game [Image]
getImages = do
  p <- use phase
  case p of
    Init -> do
      pos <- use cursorPos

      return [ translate 1 1 $ drawMenuItem (pos == 1) "Start a game."
             , translate 1 2 $ drawMenuItem (pos == 2) "Load a game."
             ]

    Loading -> do
      bs <- use buffer
      return [ translate 1 1 $ string defAttr ("Game name: " <> bs <> "|") ]

    Gaming -> do
      pos <- use cursorPos
      ply <- use player
      b   <- use board

      let playerCur = placePlayerCursorAt ply pos
          playboard = translate originX originY  boardImage
          landscape = playerCur : snoc (drawSlots b) playboard

      return landscape
    GameComplete -> do
      Just w <- use winner
      return [ placeTextAt (1,1) (show w <> " won. Press [ENTER] to quit.") ]

--------------------------------------------------------------------------------
handleComplete :: Key -> [Modifier] -> Game Bool
handleComplete KEnter _ = return False
handleComplete _ _      = return True

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

      when succeed $ do
        player %= nextPlayer

        outcome <- checkWin
        for_ outcome $ \winPlayer -> do
          winner ?= winPlayer
          phase  .= GameComplete

      return True
    _ -> return True

--------------------------------------------------------------------------------
handleMenu :: Key -> [Modifier] -> Game Bool
handleMenu (KChar 'c') [MCtrl] = return False
handleMenu key _ = do
  pos <- use cursorPos
  case key of
    KUp ->
      when (pos - 1 >= 1) $ do
        cursorPos -= 1

    KDown ->
      when (pos + 1 <= 2) $ do
        cursorPos += 1

    KEnter ->
      if pos == 1
      then phase .= Gaming
      else phase .= Loading

    _ -> return ()

  return True

--------------------------------------------------------------------------------
handleLoad :: Key -> [Modifier] -> Game Bool
handleLoad (KChar 'c') [MCtrl] = return False
handleLoad key _ = do
  case key of
    KChar c -> buffer %= \bs -> snoc bs c
    KBS     -> buffer %= \bs -> fromMaybe bs (initMay bs)
    KEnter  -> phase .= Gaming
    _       -> print key

  return True

--------------------------------------------------------------------------------
loadGame :: StreamName -> Game ()
loadGame stream = do
  put newGameState

  phase .= Gaming

  store <- getStore
  _ <- runExceptT $ forEvents store stream $ \(MovePlayed p pos) -> do
    _ <- insertToken pos
    player .= nextPlayer p

  return ()