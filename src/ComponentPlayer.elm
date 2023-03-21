module ComponentPlayer exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier


type alias ComponentPlayer =
    {}


emptyTable : ComponentTable ComponentPlayer
emptyTable =
    ComponentTable.empty
        { toString =
            \_ ->
                "ComponentPlayer()"
        }


modifier =
    Modifier.tableModifier
        { get = .playerComponents
        , set = \table world -> { world | playerComponents = table }
        }


identity : ComponentPlayer
identity =
    {}
