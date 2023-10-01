module Main exposing (main)

import Browser
import Event exposing (Msg(..))
import Html exposing (Html)
import Html.Attributes as HA
import WorldLevel1
import System
import SystemUpdateVisual
import World exposing (World)
import WorldSystem


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
            WorldLevel1.init
    in
    ( System.run SystemUpdateVisual.updateEntity level1.entities level1
    , Cmd.none
    )



-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    WorldSystem.ops.update msg world



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
            [ WorldSystem.ops.view world
            ]
        ]
    }



-- Subscriptions


subscriptions : World -> Sub Msg
subscriptions world =
    WorldSystem.ops.subscriptions world
