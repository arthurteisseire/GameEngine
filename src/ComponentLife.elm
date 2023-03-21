module ComponentLife exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier


type alias ComponentLife =
    { healPoints : Int
    }


emptyTable : ComponentTable ComponentLife
emptyTable =
    ComponentTable.empty
        { toString =
            \lifeA ->
                "Life(healPoints = "
                    ++ String.fromInt lifeA.healPoints
                    ++ ")"
        }


modifier =
    Modifier.tableModifier
        { get = .lifeComponents
        , set = \table world -> { world | lifeComponents = table }
        }


identity : ComponentLife
identity =
    { healPoints = 0
    }
