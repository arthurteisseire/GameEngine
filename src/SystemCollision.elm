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



--updateWorld : World -> World
--updateWorld world =
--    updateEntitiesWithOthers
--        { updateComponents = collide
--        , updateWorld =
--            \entityId { position, velocity } accWorld ->
--                { accWorld
--                    | positionComponents = setComponent entityId position accWorld.positionComponents
--                    , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
--                }
--        , world = world
--        , entityTable = world.entities
--        , readTable =
--            InputComponents
--                |> from world.positionComponents
--                |> fullJoin world.velocityComponents
--        , writeTable =
--            OutputComponents
--                |> from world.positionComponents
--                |> join world.velocityComponents
--        }


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
collide entityId inputComponents { position, velocity } =
    let
        entityMovedPos =
            { x = position.x + velocity.x
            , y = position.y + velocity.y
            }

        otherMovedPos =
            mapTable
                (\inputEntityId input ->
                    if inputEntityId /= entityId then
                        case input.maybeVelocity of
                            Just inputVelocity ->
                                { x = input.position.x + inputVelocity.x
                                , y = input.position.y + inputVelocity.y
                                }

                            Nothing ->
                                input.position

                    else
                        input.position
                )
                inputComponents

        nextPos =
            if List.member entityMovedPos (valuesTable otherMovedPos) then
                position

            else
                entityMovedPos
    in
    { position = nextPos
    , velocity = ComponentVelocity.identity
    }
