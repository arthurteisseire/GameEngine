module SystemDisplayDebug exposing (..)

import Core.EntityId as EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Showable exposing (Showable)
import Svg.Attributes as SA
import Svg.Events as SE
import World exposing (World)


debugComponents : EntityId -> World -> List (Html Msg)
debugComponents entityId world =
    let
        componentToHtml : Showable a -> (World -> Table a) -> List (Html Msg) -> List (Html Msg)
        componentToHtml showable getTable html =
            case Table.get entityId (getTable world) of
                Just comp ->
                    html ++ [ Html.text (showable.toString comp) ]

                Nothing ->
                    html
    in
    [ Html.text ("EntityId(" ++ EntityId.toString entityId ++ ")") ]
        |> componentToHtml Showable.keyboardInput .keyboardInputComponents
        |> componentToHtml Showable.visual .visualComponents
        |> componentToHtml Showable.position .positionComponents
        |> componentToHtml Showable.velocity .velocityComponents
        |> componentToHtml Showable.life .lifeComponents
        |> componentToHtml Showable.attack .attackComponents
        |> componentToHtml Showable.damage .damageComponents
        |> componentToHtml Showable.animation .animationComponents
        |> componentToHtml Showable.turn .turnComponents
        |> componentToHtml Showable.terrain .terrainComponents
        |> componentToHtml Showable.ai .aiComponents
        |> componentToHtml Showable.player .playerComponents


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
                (debugComponents entityId world)
            )
        ]
