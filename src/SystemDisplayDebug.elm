module SystemDisplayDebug exposing (..)

import Core.EntityId as EntityId exposing (EntityId)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Svg.Attributes as SA
import Svg.Events as SE
import World exposing (World)


debugComponents : EntityId -> World -> List (Html Msg)
debugComponents entityId world =
    [ Html.text ("EntityId(" ++ EntityId.toString entityId ++ ")") ]
        ++ List.map Html.text (World.toStrings entityId world)


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
