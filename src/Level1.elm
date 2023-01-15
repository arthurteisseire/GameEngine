module Level1 exposing (..)

import ComponentAI
import ComponentAnimation
import ComponentAttack
import ComponentDamage
import ComponentPlayer
import ComponentPosition exposing (ComponentPosition)
import ComponentTerrain exposing (ComponentTerrain)
import ComponentVelocity
import ComponentVisual
import EntityTable exposing (..)
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
            addEntity emptyEntitySet

        ( entities2, terrain ) =
            addEntity entities1

        ( entities3, enemies ) =
            addNEntities 20 entities2
    in
    { entities =
        entities3
    , keyboardInputComponents =
        emptyTable
    , positionComponents =
        emptyTable
            |> insertComponent playerId { x = 6, y = 6 }
            |> insertComponent terrain { x = 0, y = 0 }
            |> (\posTable ->
                    List.foldl
                        (\( entityId, pos ) table -> insertComponent entityId pos table)
                        posTable
                        (List.indexedMap
                            (\idx entityId -> ( entityId, { x = toFloat (modBy 5 idx), y = toFloat (idx // 5) } ))
                            enemies
                        )
               )
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
            |> (\visualTable ->
                    List.foldl
                        (\entityId table -> insertComponent entityId (ComponentVisual.circle "orange") table)
                        visualTable
                        enemies
               )
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
            |> insertComponent playerId ComponentPlayer.identity
    , terrainComponents =
        emptyTable
            |> insertComponent terrain { dimensions = { x = 16, y = 16 }, sizeFactor = 50 }
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
        [ HA.id "Level1"
        , HA.style "float" "left"
        ]
        [ screen
        ]
