module World exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import ComponentAttack exposing (ComponentAttack)
import ComponentVelocity exposing (ComponentVelocity)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)


type alias World =
    { entities : EntityTable
    , keyboardInputComponents : Table ComponentKeyboardInput
    , positionComponents : Table ComponentPosition
    , velocityComponents : Table ComponentVelocity
    , lifeComponents : Table ComponentLife
    , visualComponents : Table ComponentVisual
    , attackComponents : Table ComponentAttack
    , entityIdDebug : Maybe EntityId
    }


init : World
init =
    let
        ( entities, playerId ) =
            addEntity emptyEntityTable

        ( entities2, enemyId ) =
            addEntity entities

        keyboardInputComponents =
            emptyTable
                |> setComponent playerId ComponentKeyboardInput.identity

        positionComponents =
            emptyTable
                |> setComponent playerId { x = 4, y = 0 }
                |> setComponent enemyId { x = 5, y = 0 }

        velocityComponents =
            emptyTable
                |> setComponent playerId ComponentVelocity.identity

        lifeComponents =
            emptyTable
                |> setComponent playerId ComponentLife.identity
                |> setComponent enemyId ComponentLife.identity

        visualComponents =
            emptyTable
                |> setComponent playerId ComponentVisual.defaultRect
                |> setComponent enemyId ComponentVisual.defaultCircle

        attackComponents =
            emptyTable
                |> setComponent playerId ComponentAttack.identity
    in
    { entities = entities2
    , keyboardInputComponents = keyboardInputComponents
    , positionComponents = positionComponents
    , velocityComponents = velocityComponents
    , lifeComponents = lifeComponents
    , visualComponents = visualComponents
    , attackComponents = attackComponents
    , entityIdDebug = Just playerId
    }
