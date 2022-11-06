module Level1 exposing (..)

import ComponentAI
import ComponentAnimation
import ComponentAttack
import ComponentDamage
import ComponentKeyboardInput
import ComponentPlayer
import ComponentPosition
import ComponentVelocity
import ComponentVisual
import EntityTable exposing (EntityId, Table, addEntity, emptyEntitySet, emptyTable, getComponent, insertComponent, mapEntitySet)
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
            |> insertComponent playerId ComponentPlayer.identity
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


visual : World -> Html Msg
visual world =
    let
        terrains =
            mapEntitySet
                (\entityId ->
                    Maybe.withDefault (Html.text "") <|
                        Maybe.map2
                            (\terrain position ->
                                Svg.svg
                                    [ SA.transform <|
                                        "translate("
                                            ++ String.fromFloat position.currentPos.x
                                            ++ ", "
                                            ++ String.fromFloat position.currentPos.y
                                            ++ ")"
                                    , SA.width <| String.fromInt (terrain.dimensions.x * terrain.sizeFactor)
                                    , SA.height <| String.fromInt (terrain.dimensions.y * terrain.sizeFactor)
                                    , SA.viewBox
                                        ("0 0 "
                                            ++ String.fromInt terrain.dimensions.x
                                            ++ " "
                                            ++ String.fromInt terrain.dimensions.y
                                        )
                                    ]
                                    (SystemDraw.visualToSvg world.visualComponents world.entities)
                            )
                            (getComponent entityId world.terrainComponents)
                            (getComponent entityId world.positionComponents)
                )
                world.entities
    in
    Html.div
        [ HA.id "Level1"
        , HA.style "float" "left"
        ]
        terrains
