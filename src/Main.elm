module Main exposing (main)

import Browser
import Browser.Events
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import KeyboardInput exposing (Key, keyDecoder)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import SystemAcceleration
import SystemAttack
import SystemCollision
import World exposing (World)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Init


init : () -> ( World, Cmd Msg )
init _ =
    ( World.init
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float
    | KeyBoardInput Key
    | Clicked -- TODO: Move Clicked Msg to VisualComponent ?
    | DisplayDebug EntityId
    | HideDebug


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick dt ->
            let
                worldAfterAcceleration =
                    SystemAcceleration.updateWorld world

                tablesAfterCollision =
                    SystemCollision.update
                        world.entities
                        world.positionComponents
                        world.positionComponents
                        worldAfterAcceleration.velocityComponents
            in
            ( { world
                | positionComponents = tablesAfterCollision.first
                , velocityComponents = tablesAfterCollision.second
                , keyboardInputComponents = mapEntities1 (\_ _ -> { key = Nothing }) world.entities world.keyboardInputComponents
              }
            , Cmd.none
            )

        KeyBoardInput key ->
            ( { world
                | keyboardInputComponents = mapEntities1 (\_ _ -> { key = Just key }) world.entities world.keyboardInputComponents
              }
            , Cmd.none
            )

        DisplayDebug entityId ->
            ( { world | entityIdDebug = Just entityId }
            , Cmd.none
            )

        HideDebug ->
            ( { world | entityIdDebug = Nothing }
            , Cmd.none
            )

        Clicked ->
            ( world, Cmd.none )



-- View


view : World -> Browser.Document Msg
view world =
    { title = "ECS"
    , body =
        [ Html.div
            [ HA.id "MainWindow"
            , HA.style "width" "100%"
            , HA.style "height" "600px"
            ]
            [ Html.div
                [ HA.id "Game"
                , HA.style "float" "left"
                ]
                [ Svg.svg
                    [ SA.width "300"
                    , SA.height "300"
                    , SA.viewBox "0 0 10 10"
                    ]
                    (systemDraw world.entities world.visualComponents world.positionComponents)
                ]
            , systemDisplayDebug world world.entityIdDebug
            ]
        ]
    }


systemDraw :
    EntityTable
    -> Table ComponentVisual
    -> Table ComponentPosition
    -> List (Svg Msg)
systemDraw =
    foldlEntities2 (\entityId visual position list -> toSvg entityId visual position :: list) []


toSvg : EntityId -> ComponentVisual -> ComponentPosition -> Svg Msg
toSvg entityId visual position =
    Svg.map
        (\visualMsg ->
            if visualMsg == ComponentVisual.Clicked then
                DisplayDebug entityId

            else
                Clicked
        )
        (visual.shape
            (visual.attributes ++ visual.posToAttributes position.x position.y)
            []
        )


systemDisplayDebug : World -> Maybe EntityId -> Html Msg
systemDisplayDebug world maybeEntityId =
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
                , SA.fill "blue"
                ]
                [ Html.text "Hide" ]

        componentsDebug =
            [ Html.text ("EntityId(" ++ entityIdToString entityId ++ ")")
            , case getComponent entityId world.keyboardInputComponents of
                Just _ ->
                    Html.text "KeyboardInput()"

                Nothing ->
                    Html.text ""
            , case getComponent entityId world.visualComponents of
                Just _ ->
                    Html.text "Visual()"

                Nothing ->
                    Html.text ""
            , case getComponent entityId world.positionComponents of
                Just position ->
                    Html.text
                        ("Position(x = "
                            ++ String.fromInt position.x
                            ++ ", y = "
                            ++ String.fromInt position.y
                            ++ ")"
                        )

                Nothing ->
                    Html.text ""
            , case getComponent entityId world.velocityComponents of
                Just velocity ->
                    Html.text
                        ("Velocity(x = "
                            ++ String.fromInt velocity.x
                            ++ ", y = "
                            ++ String.fromInt velocity.y
                            ++ ")"
                        )

                Nothing ->
                    Html.text ""
            , case getComponent entityId world.lifeComponents of
                Just life ->
                    Html.text
                        ("Life(healPoints = "
                            ++ String.fromInt life.healPoints
                            ++ ")"
                        )

                Nothing ->
                    Html.text ""
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



-- Subscriptions


subscriptions : World -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
        , Sub.map KeyBoardInput (Browser.Events.onKeyDown keyDecoder)
        ]
