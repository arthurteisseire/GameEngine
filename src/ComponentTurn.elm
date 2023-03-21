module ComponentTurn exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier


type alias ComponentTurn =
    { turnsToPlay : Int
    , remainingTurns : Int
    }


emptyTable : ComponentTable ComponentTurn
emptyTable =
    ComponentTable.empty
        { toString =
            \turnA ->
                "ComponentTurn(remainingTurns=" ++ String.fromInt turnA.remainingTurns ++ ")"
        }


modifier =
    Modifier.tableModifier
        { get = .turnComponents
        , set = \table world -> { world | turnComponents = table }
        }


identity : ComponentTurn
identity =
    let
        turns =
            0
    in
    { turnsToPlay = turns
    , remainingTurns = turns
    }
