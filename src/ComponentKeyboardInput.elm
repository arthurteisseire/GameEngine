module ComponentKeyboardInput exposing (..)

import KeyboardInput exposing (Key)


type alias ComponentKeyboardInput =
    { key : Maybe Key
    }


identity : ComponentKeyboardInput
identity =
    { key = Nothing
    }


toString : ComponentKeyboardInput -> String
toString input =
    case input.key of
        Just key ->
            "KeyboardInput(" ++ KeyboardInput.toString key ++ ")"

        Nothing ->
            "KeyboardInput()"
