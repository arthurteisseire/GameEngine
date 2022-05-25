module ComponentVisual exposing (ComponentVisual, init)


type alias ComponentVisual =
    { color : String
    }


init : String -> ComponentVisual
init color =
    { color = color
    }
