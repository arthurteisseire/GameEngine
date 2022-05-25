module KeyboardInput exposing (Key(..), keyDecoder)

import Json.Decode as Decode


type Key
    = Left
    | Right
    | Up
    | Down
    | Other


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Key
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            -- TODO: Wrap Keys to avoid the "ArrowUp -> Down"
            Down

        "ArrowDown" ->
            Up

        _ ->
            Other
