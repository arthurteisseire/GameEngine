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

        ( entities2, enemy1Id ) =
            addEntity entities

        ( entities3, enemy2Id ) =
            addEntity entities2

        ( entities4, enemy3Id ) =
            addEntity entities3

        enemies =
            [ enemy1Id, enemy2Id, enemy3Id ]

        keyboardInputComponents =
            emptyTable
                |> insertComponent playerId ComponentKeyboardInput.identity

        positionComponents =
            emptyTable
                |> insertComponent playerId (ComponentPosition.init { x = 4, y = 0 })
                |> insertComponent enemy1Id (ComponentPosition.init { x = 5, y = 0 })
                |> insertComponent enemy2Id (ComponentPosition.init { x = 0, y = 0 })
                |> insertComponent enemy3Id (ComponentPosition.init { x = 3, y = 3 })

        velocityComponents =
            emptyTable
                |> insertComponent playerId ComponentVelocity.identity
                |> insertComponentForEntities enemies ComponentVelocity.identity

        lifeComponents =
            emptyTable
                |> insertComponent playerId { healPoints = 1 }
                |> insertComponentForEntities enemies { healPoints = 5 }

        visualComponents =
            emptyTable
                |> insertComponent playerId ComponentVisual.defaultRect
                |> insertComponentForEntities enemies ComponentVisual.defaultCircle

        attackComponents =
            emptyTable
                |> insertComponent playerId ComponentAttack.identity
                |> insertComponentForEntities enemies ComponentAttack.identity

        damageComponents =
            emptyTable
                |> insertComponent playerId ComponentDamage.identity
                |> insertComponentForEntities enemies ComponentDamage.identity

        animationComponents =
            emptyTable
                |> insertComponent playerId ComponentAnimation.identity
                |> insertComponentForEntities enemies ComponentAnimation.identity

        turnComponents =
            emptyTable
                |> insertComponent playerId { turnsToPlay = 0, remainingTurns = 0 }
                |> insertComponentForEntities enemies { turnsToPlay = 3, remainingTurns = 3 }

        aiComponents =
            emptyTable
                |> insertComponentForEntities enemies ComponentAI.identity

        playerComponents =
            emptyTable
                |> insertComponent playerId ComponentPlayer
    in
    { entities = entities4
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


insertComponentForEntities : List EntityId -> a -> Table a -> Table a
insertComponentForEntities entities component table =
    List.foldl
        (\entity accTable -> insertComponent entity component accTable)
        table
        entities
