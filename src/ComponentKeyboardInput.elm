module ComponentKeyboardInput exposing (..)

import KeyboardInput exposing (Key)


type alias ComponentKeyboardInput =
    { key : Maybe Key
    }


identity : ComponentKeyboardInput
identity =
    { key = Nothing
    }
