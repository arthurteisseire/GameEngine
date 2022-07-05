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
import SystemAcceleration exposing (systemAcceleration)
import SystemCollision exposing (systemCollision)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { entities : EntityTable
    , keyboardInputComponents : Table ComponentKeyboardInput
    , positionComponents : Table ComponentPosition
    , velocityComponents : Table ComponentVelocity
    , lifeComponents : Table ComponentLife
    , visualComponents : Table ComponentVisual
    , entityIdDebug : Maybe EntityId
    }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( entities, playerId ) =
            addEntity emptyEntityTable

        ( entities2, enemyId ) =
            addEntity entities

        keyboardInputComponents =
            emptyComponentTable
                |> setComponent playerId ComponentKeyboardInput.identity

        positionComponents =
            emptyComponentTable
                |> setComponent playerId (ComponentPosition.mapX (\_ -> 4) ComponentPosition.identity)
                |> setComponent enemyId (ComponentPosition.mapX (\_ -> 5) ComponentPosition.identity)

        velocityComponents =
            emptyComponentTable
                |> setComponent playerId ComponentVelocity.identity

        lifeComponents =
            emptyComponentTable
                |> setComponent playerId ComponentLife.identity
                |> setComponent enemyId ComponentLife.identity

        visualComponents =
            emptyComponentTable
                |> setComponent playerId (ComponentVisual.init "blue")
                |> setComponent enemyId (ComponentVisual.init "red")
    in
    ( { entities = entities2
      , keyboardInputComponents = keyboardInputComponents
      , positionComponents = positionComponents
      , velocityComponents = velocityComponents
      , lifeComponents = lifeComponents
      , visualComponents = visualComponents
      , entityIdDebug = Just playerId
      }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float
    | KeyBoardInput Key
    | Clicked -- TODO: Move Clicked Msg to VisualComponent ?
    | DisplayDebug EntityId
    | HideDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( _, velocityComponentsAfterAcceleration ) =
                    systemAcceleration model.entities ( model.keyboardInputComponents, model.velocityComponents )

                ( positionComponentsAfterCollision, velocityComponentsAfterCollision ) =
                    systemCollision dt model.entities ( model.positionComponents, velocityComponentsAfterAcceleration )
            in
            ( { model
                | positionComponents = positionComponentsAfterCollision
                , velocityComponents = velocityComponentsAfterCollision
                , keyboardInputComponents = mapComponents (\_ _ -> { key = Nothing }) model.keyboardInputComponents
              }
            , Cmd.none
            )

        KeyBoardInput key ->
            ( { model
                | keyboardInputComponents = mapComponents (\_ _ -> { key = Just key }) model.keyboardInputComponents
              }
            , Cmd.none
            )

        DisplayDebug entityId ->
            ( { model | entityIdDebug = Just entityId }
            , Cmd.none
            )

        HideDebug ->
            ( { model | entityIdDebug = Nothing }
            , Cmd.none
            )

        Clicked ->
            ( model, Cmd.none )


damageSystem : Float -> EntityTable -> ( Table ComponentPosition, Table ComponentLife ) -> ( Table ComponentPosition, Table ComponentLife )
damageSystem dt entityTable componentTables =
    foldlEntityTable
        (mapTable2 (updateDamageSystem dt))
        componentTables
        entityTable


updateDamageSystem : Float -> ComponentPosition -> ComponentLife -> ( ComponentPosition, ComponentLife )
updateDamageSystem dt position life =
    ( position, ComponentLife.mapHp (\hp -> hp - 1) life )



-- View


view : Model -> Browser.Document Msg
view model =
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
                    (systemDraw model.entities ( model.visualComponents, model.positionComponents ))
                ]
            , systemDisplayDebug model model.entityIdDebug
            ]
        ]
    }


systemDisplayDebug : Model -> Maybe EntityId -> Html Msg
systemDisplayDebug model maybeEntityId =
    case maybeEntityId of
        Just entityId ->
            displayDebug model entityId

        Nothing ->
            Html.text ""


displayDebug : Model -> EntityId -> Html Msg
displayDebug model entityId =
    let
        hideButton =
            Html.button
                [ SE.onClick HideDebug
                , SA.fill "blue"
                ]
                [ Html.text "Hide" ]

        componentsDebug =
            [ Html.text ("EntityId(" ++ entityIdToString entityId ++ ")")
            , case getComponent model.keyboardInputComponents entityId of
                Just _ ->
                    Html.text "KeyboardInput()"

                Nothing ->
                    Html.text ""
            , case getComponent model.visualComponents entityId of
                Just visual ->
                    Html.text
                        ("Visual(color = "
                            ++ visual.color
                            ++ ")"
                        )

                Nothing ->
                    Html.text ""
            , case getComponent model.positionComponents entityId of
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
            , case getComponent model.velocityComponents entityId of
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
            , case getComponent model.lifeComponents entityId of
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


systemDraw : EntityTable -> ( Table ComponentVisual, Table ComponentPosition ) -> List (Svg Msg)
systemDraw entityTable componentTables =
    List.filterMap
        identity
        (mapEntityTable
            (\entityId -> map2Component (\visual position -> Svg.map (clickedToDisplayDebug entityId) (toSvg visual position)) componentTables entityId)
            entityTable
        )


clickedToDisplayDebug : EntityId -> Msg -> Msg
clickedToDisplayDebug entityId msg =
    case msg of
        Clicked ->
            DisplayDebug entityId

        _ ->
            msg


toSvg : ComponentVisual -> ComponentPosition -> Svg Msg
toSvg visual position =
    Svg.rect
        [ SA.x <| String.fromInt position.x
        , SA.y <| String.fromInt position.y
        , SA.width "1"
        , SA.height "1"
        , SA.fill visual.color
        , SE.onClick Clicked
        ]
        []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))
        , Sub.map KeyBoardInput (Browser.Events.onKeyDown keyDecoder)
        ]
