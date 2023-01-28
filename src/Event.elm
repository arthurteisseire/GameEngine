module Event exposing (..)

import Core.EntityId exposing (EntityId)
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
