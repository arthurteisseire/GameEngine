module SystemDisplayDebug exposing (..)

import ComponentAI
import ComponentAnimation
import ComponentAttack
import ComponentDamage
import ComponentKeyboardInput
import ComponentLife
import ComponentPlayer
import ComponentPosition
import ComponentTurn
import ComponentVelocity
import ComponentVisual
import EntityTable exposing (EntityId, Table, entityIdToString, getComponent)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Svg.Attributes as SA
import Svg.Events as SE
import World exposing (World)


display : World -> Maybe EntityId -> Html Msg
display world maybeEntityId =
    case maybeEntityId of
        Just entityId ->
            displayDebug world entityId

        Nothing ->
            Html.text ""


displayDebug : World -> EntityId -> Html Msg
displayDebug world entityId =
    let
        hideButton =
            Html.button
                [ SE.onClick HideDebug
                , SA.style "background-color: #b3b3b3; border-color: #cccccc;"
                ]
                [ Html.text "Hide" ]

        componentToHtml : Table a -> (a -> String) -> Html Msg
        componentToHtml table toStr =
            case getComponent entityId table of
                Just comp ->
                    Html.text (toStr comp)

                Nothing ->
                    Html.text ""

        componentsDebug =
            [ Html.text ("EntityId(" ++ entityIdToString entityId ++ ")")
            , componentToHtml world.keyboardInputComponents ComponentKeyboardInput.toString
            , componentToHtml world.visualComponents ComponentVisual.toString
            , componentToHtml world.positionComponents ComponentPosition.toString
            , componentToHtml world.velocityComponents ComponentVelocity.toString
            , componentToHtml world.lifeComponents ComponentLife.toString
            , componentToHtml world.attackComponents ComponentAttack.toString
            , componentToHtml world.damageComponents ComponentDamage.toString
            , componentToHtml world.animationComponents ComponentAnimation.toString
            , componentToHtml world.turnComponents ComponentTurn.toString
            , componentToHtml world.aiComponents ComponentAI.toString
            , componentToHtml world.playerComponents ComponentPlayer.toString
            ]
    in
    Html.div
        [ HA.id "DisplayDebug"
        , HA.style "width" "30%"
        , HA.style "height" "100%"
        , HA.style "float" "right"
        , HA.style "border-style" "solid"
        , HA.style "border-width" "1px"
        ]
        [ hideButton
        , Html.div
            [ HA.id "ComponentsDebug"
            , HA.style "height" "100%"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "justify-content" "start"
            ]
            (List.map
                (\componentDebug ->
                    Html.section
                        [ HA.style "border-style" "solid"
                        , HA.style "border-width" "1px"

                        --, HA.style "height" "10%"
                        ]
                        [ componentDebug ]
                )
                componentsDebug
            )
        ]
