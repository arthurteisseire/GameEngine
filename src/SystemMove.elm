module SystemMove exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import Vector2
import World exposing (..)


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OutputComponents =
    { position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity =
    Db.updateComponents
        { func = move
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentPosition.modifier.get
                |> Component.join ComponentVelocity.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentPosition.modifier.map, .position )
        }


move : InputComponents -> OutputComponents
move { position, velocity } =
    { position = Vector2.add position velocity
    }
