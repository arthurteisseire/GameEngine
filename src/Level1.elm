module Level1 exposing (..)

import EntityTable exposing (getComponent, mapEntitySet)
import Event exposing (Msg)
import Html exposing (Html)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import SystemDraw
import World exposing (World)


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
