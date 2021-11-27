module ComponentPosition exposing (ComponentPosition, identity)


type ComponentPosition
    = ComponentPosition Internals


type alias Internals =
    { x : Int
    }


identity : ComponentPosition
identity = ComponentPosition { x = 0 }

