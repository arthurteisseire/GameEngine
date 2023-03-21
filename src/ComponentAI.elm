module ComponentAI exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier exposing (Modifier)


type alias ComponentAI =
    {}


identity : ComponentAI
identity =
    {}


emptyTable : ComponentTable ComponentAI
emptyTable =
    ComponentTable.empty
        { toString = \_ -> "AI()"
        }


modifier =
    Modifier.tableModifier
        { get = .aiComponents
        , set = \table world -> { world | aiComponents = table }
        }
