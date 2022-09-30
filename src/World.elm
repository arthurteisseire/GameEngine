module World exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)


type alias World =
    { entities : EntitySet
    , keyboardInputComponents : Table ComponentKeyboardInput
    , positionComponents : Table ComponentPosition
    , velocityComponents : Table ComponentVelocity
    , lifeComponents : Table ComponentLife
    , visualComponents : Table ComponentVisual
    , attackComponents : Table ComponentAttack
    , damageComponents : Table ComponentDamage
    , animationComponents : Table ComponentAnimation
    , turnComponents : Table ComponentTurn
    , aiComponents : Table ComponentAI
    , playerComponents : Table ComponentPlayer
    , entityIdDebug : Maybe EntityId
    , isPause : Bool
    }


init : World
init =
    let
        ( entities, playerId ) =
            addEntity emptyEntitySet

        ( entities2, enemyId ) =
            addEntity entities

        keyboardInputComponents =
            emptyTable
                |> insertComponent playerId ComponentKeyboardInput.identity

        positionComponents =
            emptyTable
                |> insertComponent playerId { x = 4, y = 0 }
                |> insertComponent enemyId { x = 5, y = 0 }

        velocityComponents =
            emptyTable
                |> insertComponent playerId ComponentVelocity.identity
                |> insertComponent enemyId ComponentVelocity.identity

        lifeComponents =
            emptyTable
                |> insertComponent playerId { healPoints = 1 }
                |> insertComponent enemyId { healPoints = 5 }

        visualComponents =
            emptyTable
                |> insertComponent playerId ComponentVisual.defaultRect
                |> insertComponent enemyId ComponentVisual.defaultCircle

        attackComponents =
            emptyTable
                |> insertComponent playerId ComponentAttack.identity
                |> insertComponent enemyId ComponentAttack.identity

        damageComponents =
            emptyTable
                |> insertComponent playerId ComponentDamage.identity
                |> insertComponent enemyId ComponentDamage.identity

        animationComponents =
            emptyTable
                |> insertComponent playerId ComponentAnimation.identity
                |> insertComponent enemyId ComponentAnimation.identity

        turnComponents =
            emptyTable
                |> insertComponent playerId { turnsToPlay = 0, remainingTurns = 0 }
                |> insertComponent enemyId { turnsToPlay = 3, remainingTurns = 3 }

        aiComponents =
            emptyTable
                |> insertComponent enemyId ComponentAI.identity

        playerComponents =
            emptyTable
                |> insertComponent playerId ComponentPlayer
    in
    { entities = entities2
    , keyboardInputComponents = keyboardInputComponents
    , positionComponents = positionComponents
    , velocityComponents = velocityComponents
    , lifeComponents = lifeComponents
    , visualComponents = visualComponents
    , attackComponents = attackComponents
    , damageComponents = damageComponents
    , animationComponents = animationComponents
    , turnComponents = turnComponents
    , aiComponents = aiComponents
    , playerComponents = playerComponents
    , entityIdDebug = Just playerId
    , isPause = False
    }
