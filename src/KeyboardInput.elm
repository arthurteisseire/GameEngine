module KeyboardInput exposing (Key(..), keyDecoder)

import Json.Decode as Decode


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Space
    | KeyN
    | Other


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Key
toDirection string =
    case string of
        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        "ArrowUp" ->
            -- TODO: Wrap Keys to avoid the "ArrowUp -> Down"
            ArrowDown

        "ArrowDown" ->
            ArrowUp

        " " ->
            Space

        "n" ->
            KeyN

        _ ->
            Other
