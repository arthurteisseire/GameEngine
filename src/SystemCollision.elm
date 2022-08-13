module SystemCollision exposing (updateWorld)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import World exposing (World)


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

        -- TODO: Find a way to oblige/avoid mapEntities. Because we need to filter destroyed entities from tables
        , readTable = mapEntities1 (\_ pos -> pos) world.entities world.positionComponents
        , writeTable = intersectTable2 OutputComponents world.entities world.positionComponents world.velocityComponents
        }


collide : EntityId -> Table ComponentPosition -> OutputComponents -> OutputComponents
collide _ positionTable { position, velocity } =
    let
        movedPos =
            { x = position.x + velocity.x
            , y = position.y + velocity.y
            }

        nextPos =
            if List.member movedPos (valuesTable positionTable) then
                position

            else
                movedPos
    in
    { position = nextPos
    , velocity = ComponentVelocity.identity
    }
