module SystemMove exposing (updateEntity)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (World)


type alias InputComponents =
    { position : ComponentPosition
    , maybeVelocity : Maybe ComponentVelocity
    }


type alias OutputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map2
            (\position velocity ->
                let
                    components =
                        move entityId (OutputComponents position velocity)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.velocityComponents)


move : EntityId -> OutputComponents -> OutputComponents
move _ { position, velocity } =
    { position =
        { previousPos = position.currentPos
        , currentPos = Vector2.add position.currentPos velocity
        }
    , velocity = ComponentVelocity.identity
    }
