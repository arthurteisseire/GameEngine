module SystemCollision exposing (updateWorld)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import World exposing (World)


type alias InputComponents =
    { position : ComponentPosition
    }


type alias OutputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


updateWorld : World -> World
updateWorld world =
    updateEntitiesWithOthers
        { updateComponents = collide
        , updateWorld =
            \entityId { position, velocity } accWorld ->
                { accWorld
                    | positionComponents = setComponent entityId position accWorld.positionComponents
                    , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                }
        , world = world
        , entityTable = world.entities
        , readTable =
            InputComponents
                |> from world.positionComponents
        , writeTable =
            OutputComponents
                |> from world.positionComponents
                |> join world.velocityComponents
        }


collide : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
collide _ inputTable { position, velocity } =
    let
        movedPos =
            { x = position.x + velocity.x
            , y = position.y + velocity.y
            }

        nextPos =
            if List.member movedPos (List.map .position (valuesTable inputTable)) then
                position

            else
                movedPos
    in
    { position = nextPos
    , velocity = ComponentVelocity.identity
    }
