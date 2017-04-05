{-# LANGUAGE LambdaCase #-}
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
    TimeTravel   -> handleTimeTravel key mods

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
      streams <- use games
      pos     <- use cursorPos
      let mkList = fmap $ \(i, StreamName name) ->
            translate 1 i $ drawMenuItem (pos == i) (unpack name)

      return $ mkList (zip [1..] streams)
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

    TimeTravel -> do
      m   <- use moves
      pos <- use cursorPos
      let mkList = fmap $ \i ->
            translate 1 i $ drawMenuItem (pos == i) (show i)

      return $ mkList [1..m]

--------------------------------------------------------------------------------
handleComplete :: Key -> [Modifier] -> Game Bool
handleComplete KEnter _ = return False
handleComplete _ _      = return True

--------------------------------------------------------------------------------
handleGamingPressed :: Key -> [Modifier] -> Game Bool
handleGamingPressed (KChar 'c') [MCtrl] = return False
handleGamingPressed (KChar 't') [MCtrl] = do
  phase .= TimeTravel
  return True
handleGamingPressed key mods = do
  case key of
    KLeft -> do
      pos <- use cursorPos
      when (pos - 1 >= 1) $ do
        cursorPos -= 1

    KRight -> do
      pos <- use cursorPos
      when (pos + 1 <= 7) $ do
        cursorPos += 1

    KEnter -> do
      pos     <- use cursorPos
      succeed <- insertToken pos

      when succeed $ do
        saveMove
        moves  += 1
        player %= nextPlayer

        outcome <- checkWin
        for_ outcome $ \winPlayer -> do
          winner ?= winPlayer
          phase  .= GameComplete

    _ -> return ()

  return True

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

    KEnter -> do
      cursorPos .= 1
      loadGames

      if pos == 1
        then do
          createGame
          phase .= Gaming
        else phase .= Loading

    _ -> return ()

  return True

--------------------------------------------------------------------------------
handleTimeTravel :: Key -> [Modifier] -> Game Bool
handleTimeTravel key _ = do
  pos <- use cursorPos
  case key of
    KUp ->
      when (pos - 1 >= 1) $ do
        cursorPos -= 1

    KDown -> do
      m <- use moves
      when (pos + 1 <= m) $ do
        cursorPos += 1

    KEnter -> do
      stream <- use curGame

      curGame .= stream
      phase   .= Gaming

      loadGameAt stream pos

  return True

--------------------------------------------------------------------------------
handleLoad :: Key -> [Modifier] -> Game Bool
handleLoad (KChar 'c') [MCtrl] = return False
handleLoad key _ = do
  pos <- use cursorPos
  case key of
    KUp -> do
      when (pos - 1 >= 1) $ do
        cursorPos -= 1

    KDown -> do
      streams <- use games
      when (pos + 1 <= length streams) $ do
        cursorPos += 1

    KEnter -> do
      streams <- use games
      let stream = indexEx streams (pos-1)

      curGame .= stream
      phase   .= Gaming

      loadGame stream

    _ -> return ()

  return True

--------------------------------------------------------------------------------
loadGame :: StreamName -> Game ()
loadGame stream = do
  board .= emptyBoard

  store <- getStore
  _ <- runExceptT $ forEvents store stream $ \(MovePlayed p pos) -> do
    _ <- insertToken pos
    player    .= nextPlayer p
    cursorPos .= pos
    moves     += 1

  return ()

--------------------------------------------------------------------------------
loadGameAt :: StreamName -> Int -> Game ()
loadGameAt stream time = do
  m <- use moves
  createGame
  newGame <- use curGame
  moves .= 0
  board .= emptyBoard

  store <- getStore
  let limit  = EventNumber (fromIntegral m)
      action = forSavedEvents store stream $ \case
        SavedEvent num evt
          | num <= limit -> do
            store <- getStore
            _     <- appendEvent store newGame AnyVersion evt >>= waitAsync
            for_ (decodeEvent evt) $ \(MovePlayed p pos) -> do
              _ <- insertToken pos
              moves     += 1
              player    .= nextPlayer p
              cursorPos .= pos
          | otherwise -> return ()

  _ <- runExceptT action
  return ()

--------------------------------------------------------------------------------
loadGames :: Game ()
loadGames = do
  gameCount .= 0

  store <- getStore
  let action = foldEventsM store "games" $ \xs (GameCreated name) -> do
        gameCount += 1

        return (StreamName name:xs)

  outcome <- runExceptT (action [])
  case outcome of
    Left err      -> putStrLn $ tshow err
    Right streams -> games .= streams

--------------------------------------------------------------------------------
createGame :: Game ()
createGame = do
  count <- use gameCount

  gameCount += 1
  let name   = "game-" <> tshow (count + 1)
      stream = StreamName name

  curGame .= stream

  store <- getStore
  _     <- appendEvent store "games" AnyVersion (GameCreated name) >>= waitAsync
  return ()

--------------------------------------------------------------------------------
saveMove :: Game ()
saveMove = do
  p      <- use player
  pos    <- use cursorPos
  stream <- use curGame
  store  <- getStore

  _ <- appendEvent store stream AnyVersion (MovePlayed p pos) >>= waitAsync
  return ()