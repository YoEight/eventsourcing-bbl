--------------------------------------------------------------------------------
-- |
-- Module : Event
-- Copyright : (C) 2017 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- This module defines the events used to represent the state of the game.
--------------------------------------------------------------------------------
module Event where

--------------------------------------------------------------------------------
import ClassyPrelude
import Data.Aeson
import EventSource

--------------------------------------------------------------------------------
import Types

--------------------------------------------------------------------------------
-- | Every time a new game is created.
data GamesEvent = GameCreated StreamName

--------------------------------------------------------------------------------
instance EncodeEvent GamesEvent where
  encodeEvent (GameCreated name) = do
    setEventType "game-created"

    let payload = object [ "name" .= name ]
    setEventPayload (dataFromJson payload)

--------------------------------------------------------------------------------
instance DecodeEvent GamesEvent where
  decodeEvent e =
    case eventType e of
      "game-created" ->
        dataAsParse (eventPayload e) $ withObject "GameCreated" $ \o ->
          fmap GameCreated (o .: "name")
      _ -> Left "Unexpected event type"

--------------------------------------------------------------------------------
-- | Every time a 'Player' made a move.
data GameMoveEvent = MovePlayed Player Int

--------------------------------------------------------------------------------
instance EncodeEvent GameMoveEvent where
  encodeEvent (MovePlayed p pos) = do
    setEventType "move-played"

    let payload =
          object [ "player"   .= p
                 , "position" .= pos
                 ]
    setEventPayload (dataFromJson payload)

--------------------------------------------------------------------------------
instance DecodeEvent GameMoveEvent where
  decodeEvent e =
    case eventType e of
      "move-played" ->
        dataAsParse (eventPayload e) $ withObject "MovePlayed" $ \o ->
          MovePlayed <$> (o .: "player")
                     <*> (o .: "position")
      _ -> Left "Unexpected event type"