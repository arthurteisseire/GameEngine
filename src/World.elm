module World exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentDamage exposing (ComponentDamage)
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTerrain exposing (ComponentTerrain)
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
    , terrainComponents : Table ComponentTerrain
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

        ( entities5, terrain ) =
            addEntity entities4

        enemies =
            [ enemy1Id, enemy2Id, enemy3Id ]
    in
    { entities =
        entities5
    , keyboardInputComponents =
        emptyTable
            |> insertComponent playerId ComponentKeyboardInput.identity
    , positionComponents =
        emptyTable
            |> insertComponent playerId (ComponentPosition.init { x = 4, y = 0 })
            |> insertComponent enemy1Id (ComponentPosition.init { x = 5, y = 0 })
            |> insertComponent enemy2Id (ComponentPosition.init { x = 0, y = 0 })
            |> insertComponent enemy3Id (ComponentPosition.init { x = 3, y = 3 })
            |> insertComponent terrain (ComponentPosition.init { x = 100, y = 100 })
    , velocityComponents =
        emptyTable
            |> insertComponent playerId ComponentVelocity.identity
            |> insertComponentForEntities enemies ComponentVelocity.identity
    , lifeComponents =
        emptyTable
            |> insertComponent playerId { healPoints = 1 }
            |> insertComponentForEntities enemies { healPoints = 5 }
    , visualComponents =
        emptyTable
            |> insertComponent playerId ComponentVisual.defaultRect
            |> insertComponent enemy1Id (ComponentVisual.circle "purple")
            |> insertComponent enemy2Id (ComponentVisual.circle "orange")
            |> insertComponent enemy3Id (ComponentVisual.circle "red")
            |> insertComponent terrain ComponentVisual.terrain
    , attackComponents =
        emptyTable
            |> insertComponent playerId ComponentAttack.identity
            |> insertComponentForEntities enemies ComponentAttack.identity
    , damageComponents =
        emptyTable
            |> insertComponent playerId ComponentDamage.identity
            |> insertComponentForEntities enemies ComponentDamage.identity
    , animationComponents =
        emptyTable
            |> insertComponent playerId ComponentAnimation.identity
            |> insertComponentForEntities enemies ComponentAnimation.identity
    , turnComponents =
        emptyTable
            |> insertComponent playerId { turnsToPlay = 0, remainingTurns = 0 }
            |> insertComponentForEntities enemies { turnsToPlay = 3, remainingTurns = 3 }
    , aiComponents =
        emptyTable
            |> insertComponentForEntities enemies ComponentAI.identity
    , playerComponents =
        emptyTable
            |> insertComponent playerId ComponentPlayer
    , terrainComponents =
        emptyTable
            |> insertComponent terrain { dimensions = { x = 8, y = 8 }, sizeFactor = 100 }
    , entityIdDebug =
        Just playerId
    , isPause =
        False
    }


insertComponentForEntities : List EntityId -> a -> Table a -> Table a
insertComponentForEntities entities component table =
    List.foldl
        (\entity accTable -> insertComponent entity component accTable)
        table
        entities
