module WorldLevel1 exposing (..)

import ComponentAI
import ComponentAnimation
import ComponentAttack
import ComponentDamage
import ComponentKeyboardInput
import ComponentLife
import ComponentPlayer
import ComponentPosition exposing (ComponentPosition)
import ComponentTerrain exposing (ComponentTerrain)
import ComponentTurn
import ComponentVelocity
import ComponentVisual
import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet
import Event exposing (Msg)
import Html exposing (Html)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import SystemDraw
import World exposing (World)


init : World
init =
    let
        ( entities1, playerId ) =
            EntitySet.addEntity EntitySet.empty

        ( entities2, terrain ) =
            EntitySet.addEntity entities1

        ( entities3, enemies ) =
            EntitySet.addNEntities 20 entities2
    in
    { entities =
        entities3
    , keyboardInputComponents =
        ComponentKeyboardInput.emptyTable
    , positionComponents =
        ComponentPosition.emptyTable
            |> ComponentTable.insert playerId { x = 6, y = 6 }
            |> ComponentTable.insert terrain { x = 0, y = 0 }
            |> (\posTable ->
                    List.foldl
                        (\( entityId, pos ) table -> ComponentTable.insert entityId pos table)
                        posTable
                        (List.indexedMap
                            (\idx entityId -> ( entityId, { x = toFloat (modBy 5 idx), y = toFloat (idx // 5) } ))
                            enemies
                        )
               )
    , velocityComponents =
        ComponentVelocity.emptyTable
            |> ComponentTable.insert playerId ComponentVelocity.identity
            |> insertComponentForEntities enemies ComponentVelocity.identity
    , lifeComponents =
        ComponentLife.emptyTable
            |> ComponentTable.insert playerId { healPoints = 1 }
            |> insertComponentForEntities enemies { healPoints = 5 }
    , visualComponents =
        ComponentVisual.emptyTable
            |> ComponentTable.insert playerId ComponentVisual.defaultRect
            |> (\visualTable ->
                    List.foldl
                        (\entityId table -> ComponentTable.insert entityId (ComponentVisual.circle "orange") table)
                        visualTable
                        enemies
               )
            |> ComponentTable.insert terrain ComponentVisual.terrain
    , attackComponents =
        ComponentAttack.emptyTable
            |> ComponentTable.insert playerId ComponentAttack.identity
            |> insertComponentForEntities enemies ComponentAttack.identity
    , damageComponents =
        ComponentDamage.emptyTable
            |> ComponentTable.insert playerId ComponentDamage.identity
            |> insertComponentForEntities enemies ComponentDamage.identity
    , animationComponents =
        ComponentAnimation.emptyTable
            |> ComponentTable.insert playerId ComponentAnimation.identity
            |> insertComponentForEntities enemies ComponentAnimation.identity
    , turnComponents =
        ComponentTurn.emptyTable
            |> ComponentTable.insert playerId { turnsToPlay = 0, remainingTurns = 0 }
            |> insertComponentForEntities enemies { turnsToPlay = 3, remainingTurns = 3 }
    , aiComponents =
        ComponentAI.emptyTable
            |> insertComponentForEntities enemies ComponentAI.identity
    , playerComponents =
        ComponentPlayer.emptyTable
            |> ComponentTable.insert playerId ComponentPlayer.identity
    , terrainComponents =
        ComponentTerrain.emptyTable
            |> ComponentTable.insert terrain { dimensions = { x = 16, y = 16 }, sizeFactor = 50 }
    , entityIdDebug =
        Just playerId
    , isPause =
        False
    }


insertComponentForEntities : List EntityId -> a -> ComponentTable a -> ComponentTable a
insertComponentForEntities entities component table =
    List.foldl
        (\entity accTable -> ComponentTable.insert entity component accTable)
        table
        entities


type alias InputComponents =
    { terrain : ComponentTerrain
    , position : ComponentPosition
    }


visual : World -> Html Msg
visual world =
    let
        screenPosition =
            { x = 100, y = 100 }

        dimensions =
            { x = 16, y = 16 }

        sizeFactor =
            50

        screen =
            Svg.svg
                [ SA.transform <|
                    "translate("
                        ++ String.fromFloat screenPosition.x
                        ++ ", "
                        ++ String.fromFloat screenPosition.y
                        ++ ")"
                , SA.width <| String.fromInt (dimensions.x * sizeFactor)
                , SA.height <| String.fromInt (dimensions.y * sizeFactor)
                , SA.viewBox
                    ("0 0 "
                        ++ String.fromInt dimensions.x
                        ++ " "
                        ++ String.fromInt dimensions.y
                    )
                ]
                (SystemDraw.visualToSvg world.visualComponents world.entities)
    in
    Html.div
        [ HA.id "WorldLevel1"
        , HA.style "float" "left"
        ]
        [ screen
        ]
