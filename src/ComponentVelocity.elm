module ComponentVelocity exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Vector2 exposing (Vector2)


type alias ComponentVelocity =
    Vector2 Float


emptyTable : ComponentTable ComponentVelocity
emptyTable =
    ComponentTable.empty
        { toString =
            \velocityA ->
                "Velocity(" ++ Vector2.vectorFloatToString velocityA ++ ")"
        }


modifier =
    Modifier.tableModifier
        { get = .velocityComponents
        , set = \table world -> { world | velocityComponents = table }
        }


identity : ComponentVelocity
identity =
    { x = 0
    , y = 0
    }
