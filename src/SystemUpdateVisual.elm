module SystemUpdateVisual exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
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
    Db.updateComponents
        { func = updateVisual
        , inputComponents =
            Component.select InputComponents
                |> Component.join .visualComponents
                |> Component.join .positionComponents
        , output =
            Modifier.select
                |> Modifier.join ( visualModifier, .visual )
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
