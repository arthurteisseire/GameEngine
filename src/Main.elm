module Main exposing (main)

import Browser
import Browser.Events
import ComponentAI
import ComponentAnimation
import ComponentAttack
import ComponentDamage
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
import SystemAITurn
import SystemAcceleration
import SystemAccelerationAI
import SystemAnimation
import SystemAttack
import SystemCollision
import SystemDamage
import SystemDie
import SystemKeyboardInput
import SystemLife
import SystemTriggerAttackAnimation
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
    | TogglePause
    | NextFrame
    | KeyBoardInput Key
    | DiscardMsg -- TODO: Move Clicked Msg to VisualComponent ?
    | DisplayDebug EntityId
    | HideDebug


applySystem : (EntityId -> World -> World) -> EntitySet -> World -> World
applySystem updateEntity entitySet world =
    foldlEntitySet
        updateEntity
        world
        entitySet


applySystems : Float -> EntitySet -> World -> World
applySystems dt entitySet world =
    world
        |> applySystem SystemAcceleration.updateEntity entitySet
        |> applySystem SystemAccelerationAI.updateEntity entitySet
        |> applySystem SystemAttack.updateEntity entitySet
        |> applySystem SystemDamage.updateEntity entitySet
        |> applySystem SystemTriggerAttackAnimation.updateEntity entitySet
        |> applySystem SystemLife.updateEntity entitySet
        |> applySystem SystemCollision.updateEntity entitySet
        |> applySystem (SystemAnimation.updateEntity dt) entitySet
        |> applySystem SystemKeyboardInput.clear entitySet
        |> applySystem SystemAcceleration.clearVelocity entitySet
        |> applySystem SystemDie.updateEntity entitySet


getPlayers : World -> EntitySet
getPlayers world =
    filterEntities
        (\entityId -> getComponent entityId world.playerComponents /= Nothing)
        world.entities


getAis : World -> EntitySet
getAis world =
    filterEntities
        (\entityId -> getComponent entityId world.aiComponents /= Nothing)
        world.entities


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        KeyBoardInput key ->
            ( world
                |> applySystem (SystemKeyboardInput.read key) world.entities
                |> applySystem SystemAITurn.updateEntity world.entities
            , Cmd.none
            )

        Tick dt ->
            ( world
                |> applySystems dt (getPlayers world)
                |> applySystems dt (getAis world)
            , Cmd.none
            )

        TogglePause ->
            ( { world | isPause = not world.isPause }, Cmd.none )

        NextFrame ->
            if world.isPause then
                ( world
                    |> applySystems (1 / 60) (getPlayers world)
                    |> applySystems (1 / 60) (getAis world)
                , Cmd.none
                )

            else
                ( world, Cmd.none )

        DisplayDebug entityId ->
            ( { world | entityIdDebug = Just entityId }
            , Cmd.none
            )

        HideDebug ->
            ( { world | entityIdDebug = Nothing }
            , Cmd.none
            )

        DiscardMsg ->
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
            [ if world.isPause then Html.text "Pause" else Html.text ""
            , Html.div
                [ HA.id "Game"
                , HA.style "float" "left"
                ]
                [ Svg.svg
                    [ SA.width "500"
                    , SA.height "500"
                    , SA.viewBox "0 0 20 20"
                    ]
                    (systemDraw world.visualComponents world.entities)
                ]
            , systemDisplayDebug world world.entityIdDebug
            ]
        ]
    }


systemDraw :
    Table ComponentVisual
    -> EntitySet
    -> List (Svg Msg)
systemDraw visualTable entitySet =
    mapEntities (\entityId visual -> toSvg entityId visual) visualTable entitySet
        |> valuesTable


toSvg : EntityId -> ComponentVisual -> Svg Msg
toSvg entityId visual =
    Svg.map
        (\visualMsg ->
            if visualMsg == ComponentVisual.Clicked then
                DisplayDebug entityId

            else
                DiscardMsg
        )
        (visual.shape
            (visual.attributes ++ visual.posToAttributes visual.position)
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
            , componentToHtml world.damageComponents ComponentDamage.toString
            , componentToHtml world.animationComponents ComponentAnimation.toString
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
subscriptions world =
    Sub.batch
        [ if world.isPause then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
        , Sub.map
            (\key ->
                case key of
                    KeyboardInput.Space ->
                        TogglePause

                    KeyboardInput.KeyN ->
                        NextFrame

                    _ ->
                        KeyBoardInput key
            )
            (Browser.Events.onKeyDown keyDecoder)
        ]
