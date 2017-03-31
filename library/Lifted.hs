{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude (putStr, reads)
--------------------------------------------------------------------------------
import           ClassyPrelude hiding (putStr)
import           Control.Lens
import qualified System.Console.ANSI as ANSI

--------------------------------------------------------------------------------
import Types

--------------------------------------------------------------------------------
clearScreen :: Game ()
clearScreen = do
  -- liftIO ANSI.clearScreen
  setCursorPosition 0 0

--------------------------------------------------------------------------------
setCursorPosition :: Int -> Int -> Game ()
setCursorPosition x y = do
  liftIO $ ANSI.setCursorPosition y x

  posX .= x
  posY .= y

--------------------------------------------------------------------------------
setCursorLine :: Int -> Game ()
setCursorLine y = do
  x <- use posX
  setCursorPosition x y

--------------------------------------------------------------------------------
printSpace :: Game ()
printSpace = printChar ' '

--------------------------------------------------------------------------------
printChar :: Char -> Game ()
printChar c = do
  posX += 1
  liftIO $ putStr $ myShow c

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

--------------------------------------------------------------------------------
myShow :: Show a => a -> String
myShow x = go (show x)
  where
    go :: String -> String
    go [] = []
    go s@(x:xs) = case x of
      '\'' -> char : go rest'
      _    -> x : go xs
      where
        (char :: Char, rest'):_ = reads s