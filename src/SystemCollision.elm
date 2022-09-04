module SystemCollision exposing (updateEntity)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
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
                    inputComponents =
                        (InputComponents
                            |> from world.positionComponents
                            |> fullJoin world.velocityComponents
                        )
                            world.entities

                    components =
                        collide entityId inputComponents (OutputComponents position velocity)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.velocityComponents)


collide : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
collide _ inputComponents { position, velocity } =
    let
        entityMovedPos =
            { x = position.x + velocity.x
            , y = position.y + velocity.y
            }

        nextPos =
            if List.member entityMovedPos (List.map (\input -> input.position) (valuesTable inputComponents)) then
                position

            else
                entityMovedPos
    in
    { position = nextPos
    , velocity = ComponentVelocity.identity
    }
