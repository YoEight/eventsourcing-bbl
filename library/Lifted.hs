--------------------------------------------------------------------------------
-- |
-- Module : Lifted
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Lifted where

--------------------------------------------------------------------------------
import           ClassyPrelude
import           Control.Lens
import qualified System.Console.ANSI as ANSI

--------------------------------------------------------------------------------
import Types

--------------------------------------------------------------------------------
clearScreen :: Game ()
clearScreen = do
  liftIO $ do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0

  posX .= 0
  posY .= 0 

--------------------------------------------------------------------------------
printSpace :: Game ()
printSpace = printChar ' '

--------------------------------------------------------------------------------
printChar :: Char -> Game ()
printChar c = do
  posX += 1
  putStr $ pack [c]

--------------------------------------------------------------------------------
cursorUp :: Int -> Game ()
cursorUp i = do
  y <- use posY

  unless (y == 0) $ do
    posY -= 1
    liftIO $ ANSI.cursorUp i

--------------------------------------------------------------------------------
cursorDown :: Int -> Game ()
cursorDown i = do
  posY += 1
  liftIO $ ANSI.cursorDown i

--------------------------------------------------------------------------------
cursorForward :: Int -> Game ()
cursorForward i = do
  posX += 1
  liftIO $ ANSI.cursorForward i

--------------------------------------------------------------------------------
cursorBackward :: Int -> Game ()
cursorBackward i = do
  x <- use posX

  unless (x == 0) $ do
    posX -= 1
    liftIO $ ANSI.cursorBackward i

--------------------------------------------------------------------------------
setCursorColumn :: Int -> Game ()
setCursorColumn i = do
  posX .= i
  liftIO $ ANSI.setCursorColumn i
