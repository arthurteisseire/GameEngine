module SystemAccelerationAI exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntityWithOthers3
                updateAIVelocity
                world.entities
                (intersectTable2 world.entities world.playerComponents world.positionComponents)
                world.aiComponents
                world.velocityComponents
                world.positionComponents
    in
    { world
        | aiComponents = tables.first
        , velocityComponents = tables.second
    }


updateAIVelocity :
    EntityId
    -> Table (Tuple2 ComponentPlayer ComponentPosition)
    -> ComponentAI
    -> ComponentVelocity
    -> ComponentPosition
    -> Tuple3 ComponentAI ComponentVelocity ComponentPosition
updateAIVelocity _ table ai velocity position =
    let
        remainingTurns =
            ai.remainingTurnsBeforeMove - 1

        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map (\{ first, second } -> second) (valuesTable table)))

        diff =
            { x = playerPos.x - position.x, y = playerPos.y - position.y }

        nextVelocity =
            if abs diff.x > abs diff.y then
                { x = diff.x // abs diff.x, y = 0 }

            else
                { x = 0, y = diff.y // abs diff.y }
    in
    if remainingTurns < 0 then
        toTuple3 { ai | remainingTurnsBeforeMove = 4 } nextVelocity position

    else
        toTuple3 { ai | remainingTurnsBeforeMove = remainingTurns } velocity position
