module ComponentKeyboardInput exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import KeyboardInput exposing (Key)


type alias ComponentKeyboardInput =
    { key : Maybe Key
    }


emptyTable : ComponentTable ComponentKeyboardInput
emptyTable =
    ComponentTable.empty
        { toString =
            \keyboardInputA ->
                case keyboardInputA.key of
                    Just key ->
                        "KeyboardInput(" ++ KeyboardInput.toString key ++ ")"

                    Nothing ->
                        "KeyboardInput()"
        }


modifier =
    Modifier.tableModifier
        { get = .keyboardInputComponents
        , set = \table world -> { world | keyboardInputComponents = table }
        }


identity : ComponentKeyboardInput
identity =
    { key = Nothing
    }
