module Main exposing (main)

import Browser
import Browser.Events
import ComponentAI as ComponentAi
import ComponentPlayer
import Core.ComponentTable as ComponentTable
import Core.Context exposing (ContextOperations)
import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode
import KeyboardInput exposing (Key, keyDecoder)
import Level1
import SystemAcceleration
import SystemAccelerationAI
import SystemAnimation
import SystemAttack
import SystemCollision
import SystemDie
import SystemDisplayDebug
import SystemKeyboardInput
import SystemLife
import SystemMove
import SystemTakeDamage
import SystemTurn
import SystemUpdateVisual
import Util
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
    let
        level1 =
            Level1.init
    in
    ( Util.applySystem SystemUpdateVisual.updateEntity level1.entities level1
    , Cmd.none
    )



-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    ( updateWorld msg world, Cmd.none )


updateWorld : Msg -> World -> World
updateWorld msg world =
    case msg of
        KeyBoardInput key ->
            if world.isPause then
                world
                    |> Util.applySystem (SystemKeyboardInput.read key) world.entities

            else
                world
                    |> Util.applySystem (SystemKeyboardInput.read key) world.entities
                    |> playTurn

        Tick dt ->
            world
                |> applyTickSystems dt world.entities

        TogglePause ->
            { world | isPause = not world.isPause }

        NextFrame ->
            if world.isPause then
                playTurn world

            else
                world

        DisplayDebug entityId ->
            { world | entityIdDebug = Just entityId }

        HideDebug ->
            { world | entityIdDebug = Nothing }

        Clicked ->
            world

        DiscardMsg ->
            world


playTurn : World -> World
playTurn world =
    let
        players =
            getPlayers world

        ais =
            getAis world
    in
    world
        |> Util.applySystem SystemTurn.updateEntity world.entities
        |> Util.applySystem SystemAcceleration.updateEntity players
        |> play players
        |> Util.applySystem SystemAccelerationAI.updateEntity ais
        |> play ais


play : EntitySet -> World -> World
play playingEntities world =
    world
        |> Util.applySystem SystemAttack.updateEntity playingEntities
        |> SystemCollision.updateEntities world.entities
        |> Util.applySystem SystemMove.updateEntity world.entities
        |> Util.applySystem SystemUpdateVisual.updateEntity playingEntities
        |> Util.applySystem SystemTakeDamage.updateEntity world.entities
        |> Util.applySystem SystemLife.updateEntity world.entities
        |> Util.applySystem SystemKeyboardInput.clear playingEntities
        |> Util.applySystem SystemAcceleration.clearVelocity playingEntities
        |> Util.applySystem SystemAttack.clear playingEntities
        |> Util.applySystemWithOperations SystemDie.updateEntity world.entities World.operations


applyTickSystems : Float -> EntitySet -> World -> World
applyTickSystems dt entitySet world =
    world
        |> Util.applySystem (SystemAnimation.updateEntity dt) entitySet


getPlayers : World -> EntitySet
getPlayers world =
    EntitySet.filter
        (\entityId -> ComponentTable.get entityId (ComponentPlayer.modifier.get world) /= Nothing)
        world.entities


getAis : World -> EntitySet
getAis world =
    EntitySet.filter
        (\entityId -> ComponentTable.get entityId (ComponentAi.modifier.get world) /= Nothing)
        world.entities



-- View


view : World -> Browser.Document Msg
view world =
    { title = "ECS"
    , body =
        [ Html.div
            [ HA.id "MainWindow"
            , HA.style "width" "100%"
            , HA.style "height" "955px"
            , HA.style "background-color" "#b3b3b3"
            ]
            [ if world.isPause then
                Html.text "Pause"

              else
                Html.text ""
            , Level1.visual world
            , SystemDisplayDebug.display World.operations world world.entityIdDebug
            ]
        ]
    }



-- Subscriptions


subscriptions : World -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
        , Browser.Events.onKeyDown keyDecoderToMsgDecoder
        ]


keyDecoderToMsgDecoder : Json.Decode.Decoder Msg
keyDecoderToMsgDecoder =
    Json.Decode.map
        (\key ->
            case key of
                KeyboardInput.Space ->
                    TogglePause

                KeyboardInput.KeyN ->
                    NextFrame

                _ ->
                    KeyBoardInput key
        )
        keyDecoder
