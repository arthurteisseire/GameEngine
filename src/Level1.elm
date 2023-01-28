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
import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet
import Core.Table as Table exposing (Table)
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
        Table.empty
    , positionComponents =
        Table.empty
            |> Table.insert playerId { x = 6, y = 6 }
            |> Table.insert terrain { x = 0, y = 0 }
            |> (\posTable ->
                    List.foldl
                        (\( entityId, pos ) table -> Table.insert entityId pos table)
                        posTable
                        (List.indexedMap
                            (\idx entityId -> ( entityId, { x = toFloat (modBy 5 idx), y = toFloat (idx // 5) } ))
                            enemies
                        )
               )
    , velocityComponents =
        Table.empty
            |> Table.insert playerId ComponentVelocity.identity
            |> insertComponentForEntities enemies ComponentVelocity.identity
    , lifeComponents =
        Table.empty
            |> Table.insert playerId { healPoints = 1 }
            |> insertComponentForEntities enemies { healPoints = 5 }
    , visualComponents =
        Table.empty
            |> Table.insert playerId ComponentVisual.defaultRect
            |> (\visualTable ->
                    List.foldl
                        (\entityId table -> Table.insert entityId (ComponentVisual.circle "orange") table)
                        visualTable
                        enemies
               )
            |> Table.insert terrain ComponentVisual.terrain
    , attackComponents =
        Table.empty
            |> Table.insert playerId ComponentAttack.identity
            |> insertComponentForEntities enemies ComponentAttack.identity
    , damageComponents =
        Table.empty
            |> Table.insert playerId ComponentDamage.identity
            |> insertComponentForEntities enemies ComponentDamage.identity
    , animationComponents =
        Table.empty
            |> Table.insert playerId ComponentAnimation.identity
            |> insertComponentForEntities enemies ComponentAnimation.identity
    , turnComponents =
        Table.empty
            |> Table.insert playerId { turnsToPlay = 0, remainingTurns = 0 }
            |> insertComponentForEntities enemies { turnsToPlay = 3, remainingTurns = 3 }
    , aiComponents =
        Table.empty
            |> insertComponentForEntities enemies ComponentAI.identity
    , playerComponents =
        Table.empty
            |> Table.insert playerId ComponentPlayer.identity
    , terrainComponents =
        Table.empty
            |> Table.insert terrain { dimensions = { x = 16, y = 16 }, sizeFactor = 50 }
    , entityIdDebug =
        Just playerId
    , isPause =
        False
    }


insertComponentForEntities : List EntityId -> a -> Table a -> Table a
insertComponentForEntities entities component table =
    List.foldl
        (\entity accTable -> Table.insert entity component accTable)
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
