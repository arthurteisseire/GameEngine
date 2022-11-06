module Event exposing (..)

import EntityTable exposing (EntityId)
import KeyboardInput exposing (Key)


type Msg
    = Tick Float
    | TogglePause
    | NextFrame
    | KeyBoardInput Key
    | DiscardMsg
    | Clicked
    | DisplayDebug EntityId
    | HideDebug
