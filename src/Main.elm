module Main exposing (main)

import Browser
import Browser.Events
import ComponentAI
import ComponentAttack
import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPlayer
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
import SystemAccelerationAI
import SystemAttack
import SystemClearKeyboardInputs
import SystemCollision
import SystemDamage
import SystemDie
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
    = KeyBoardInput Key
    | Clicked -- TODO: Move Clicked Msg to VisualComponent ?
    | DisplayDebug EntityId
    | HideDebug


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        KeyBoardInput key ->
            ( world
                |> (\currentWorld -> { currentWorld | keyboardInputComponents = mapEntities (\_ _ -> { key = Just key }) currentWorld.keyboardInputComponents currentWorld.entities })
                |> SystemAccelerationAI.updateWorld
                |> SystemAcceleration.updateWorld
                |> SystemAttack.updateWorld
                |> SystemDamage.updateWorld
                |> SystemCollision.updateWorld
                |> SystemClearKeyboardInputs.updateWorld
                |> (\currentWorld -> { currentWorld | velocityComponents = mapEntities (\_ _ -> ComponentVelocity.identity) currentWorld.velocityComponents currentWorld.entities })
                |> SystemDie.updateWorld
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
                    [ SA.width "500"
                    , SA.height "500"
                    , SA.viewBox "0 0 20 20"
                    ]
                    (systemDraw world.visualComponents world.positionComponents world.entities)
                ]
            , systemDisplayDebug world world.entityIdDebug
            ]
        ]
    }


systemDraw :
    Table ComponentVisual
    -> Table ComponentPosition
    -> EntitySet
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



-- Subscriptions


subscriptions : World -> Sub Msg
subscriptions _ =
    Sub.batch
        --[ Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
        [ Sub.map KeyBoardInput (Browser.Events.onKeyDown keyDecoder)
        ]
