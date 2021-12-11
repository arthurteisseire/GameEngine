module ComponentVisual exposing (ComponentVisual, init, getColor)

type ComponentVisual =
    ComponentVisual Internals


type alias Internals =
    { color : String
    }



init : String -> ComponentVisual
init color =
    ComponentVisual
        { color = color
        }


getColor : ComponentVisual -> String
getColor (ComponentVisual internals) =
    internals.color
