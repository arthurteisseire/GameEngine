module SystemUpdateVisual exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)
import World exposing (..)


type alias OutputComponents =
    { visual : ComponentVisual
    }


type alias InputComponents =
    { visual : ComponentVisual
    , position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity =
    updateComponents
        { func = updateVisual
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .visualComponents
                |> withInput .positionComponents
        , output =
            toOutputComponents
                |> withOutput visualComponent
        }


updateVisual : InputComponents -> OutputComponents
updateVisual { visual, position } =
    { visual =
        { visual
            | position =
                { x = position.x
                , y = position.y
                }
        }
    }
