module SystemUpdateVisual exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier


type alias OutputComponents =
    { visual : ComponentVisual
    }


type alias InputComponents =
    { visual : ComponentVisual
    , position : ComponentPosition
    }


updateEntity =
    Db.updateComponents
        { func = updateVisual
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentVisual.modifier.get
                |> Component.join ComponentPosition.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentVisual.modifier.map, .visual )
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
