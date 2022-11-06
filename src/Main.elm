module Main exposing (main)

import Browser
import Browser.Events
import EntityTable exposing (..)
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
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
    ( World.init
    , Cmd.none
    )



-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        KeyBoardInput key ->
            ( let
                players =
                    getPlayers world

                ais =
                    getAis world
              in
              world
                |> applySystem (SystemKeyboardInput.read key) world.entities
                |> applySystem SystemTurn.updateEntity world.entities
                |> applySystem SystemAcceleration.updateEntity players
                |> playTurn players ais
                |> applySystem SystemAccelerationAI.updateEntity ais
                |> playTurn ais players
            , Cmd.none
            )

        Tick dt ->
            ( world
                |> applyTickSystems dt (getPlayers world)
                |> applyTickSystems dt (getAis world)
            , Cmd.none
            )

        TogglePause ->
            ( { world | isPause = not world.isPause }, Cmd.none )

        NextFrame ->
            if world.isPause then
                ( world
                  --|> applySystems (getPlayers world)
                  --|> applySystems (getAis world)
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


playTurn : EntitySet -> EntitySet -> World -> World
playTurn playingEntities otherEntities world =
    world
        |> applySystem SystemAttack.updateEntity playingEntities
        |> applySystem SystemTakeDamage.updateEntity otherEntities
        |> applySystem SystemTriggerAttackAnimation.updateEntity playingEntities
        |> applySystem SystemLife.updateEntity otherEntities
        |> SystemCollision.updateEntities playingEntities
        |> applySystem SystemKeyboardInput.clear playingEntities
        |> applySystem SystemAcceleration.clearVelocity playingEntities
        |> applySystem SystemDie.updateEntity otherEntities


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
            , HA.style "height" "600px"
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
