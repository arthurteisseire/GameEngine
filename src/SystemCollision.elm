module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (World)


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
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
                            |> join world.velocityComponents
                        )
                            world.entities
                            |> removeInTable entityId

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
collide _ otherComponents components =
    let
        otherPositions =
            mapTable
                (\_ { position, velocity } -> position.currentPos)
                otherComponents

        nextPosition =
            Vector2.add components.position.currentPos components.velocity

        movedPosition =
            if hasValueInTable nextPosition otherPositions then
                components.position.currentPos

            else
                nextPosition
    in
    { position = ComponentPosition.init movedPosition
    , velocity = Vector2.identity
    }
