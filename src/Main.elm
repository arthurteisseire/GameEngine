module Main exposing (main)

import Browser
import Browser.Events
import EntityTable exposing (..)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
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
import SystemTakeDamage
import SystemTriggerAttackAnimation
import SystemTurn
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
    ( Level1.init
    , Cmd.none
    )



-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        KeyBoardInput key ->
            if world.isPause then
                ( world
                    |> applySystem (SystemKeyboardInput.read key) world.entities
                , Cmd.none
                )

            else
                ( world
                    |> applySystem (SystemKeyboardInput.read key) world.entities
                    |> playTurn
                , Cmd.none
                )

        Tick dt ->
            ( world
                |> applyTickSystems dt world.entities
            , Cmd.none
            )

        TogglePause ->
            ( { world | isPause = not world.isPause }, Cmd.none )

        NextFrame ->
            if world.isPause then
                ( playTurn world
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

        Clicked ->
            ( world, Cmd.none )

        DiscardMsg ->
            ( world, Cmd.none )


playTurn : World -> World
playTurn world =
    let
        players =
            getPlayers world

        ais =
            getAis world
    in
    world
        |> applySystem SystemTurn.updateEntity world.entities
        |> applySystem SystemAcceleration.updateEntity players
        |> confront players
        |> applySystem SystemAccelerationAI.updateEntity ais
        |> confront ais


confront : EntitySet -> World -> World
confront playingEntities world =
    world
        |> applySystem SystemAttack.updateEntity playingEntities
        |> SystemCollision.updateEntities world.entities
        |> applySystem SystemTakeDamage.updateEntity world.entities
        |> applySystem SystemLife.updateEntity world.entities
        |> applySystem SystemTriggerAttackAnimation.updateEntity world.entities
        |> applySystem SystemKeyboardInput.clear playingEntities
        |> applySystem SystemAcceleration.clearVelocity playingEntities
        |> applySystem SystemAttack.clear playingEntities
        |> applySystem SystemDie.updateEntity world.entities


applyTickSystems : Float -> EntitySet -> World -> World
applyTickSystems dt entitySet world =
    world
        |> applySystem (SystemAnimation.updateEntity dt) entitySet


applySystem : (EntityId -> World -> World) -> EntitySet -> World -> World
applySystem updateEntity entitySet world =
    foldlEntitySet
        updateEntity
        world
        entitySet


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
            , SystemDisplayDebug.display world world.entityIdDebug
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
