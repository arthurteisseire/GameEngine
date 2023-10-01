module SystemDisplayDebug exposing (..)

import Core.EntityId as EntityId exposing (EntityId)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg.Attributes as SA
import Svg.Events as SE
import World exposing (World)


debugComponents contextOperations entityId context =
    [ Html.text ("EntityId(" ++ EntityId.toString entityId ++ ")") ]
        ++ List.map Html.text (contextOperations.toStrings entityId context)


display contextOperations context maybeEntityId =
    case maybeEntityId of
        Just entityId ->
            displayDebug contextOperations context entityId

        Nothing ->
            Html.text ""


displayDebug contextOperations context entityId =
    let
        inputEntity =
            Html.input
                [ SA.type_ "float"
                , HE.onInput
                    (\str ->
                        case String.toInt str of
                            Nothing ->
                                DiscardMsg

                            Just id ->
                                DisplayDebug (EntityId.fromInt id)
                    )
                ]
                []

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
        [ inputEntity
        , hideButton
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
                (debugComponents contextOperations entityId context)
            )
        ]
