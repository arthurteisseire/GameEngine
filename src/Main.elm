module Main exposing (main)

import Browser
import Browser.Events
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { entities : EntityTable
    , positionComponents : Table ComponentPosition
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

        positionComponents =
            emptyComponentTable
                |> setComponent playerId (ComponentPosition.mapX (\_ -> 4) ComponentPosition.identity)
                |> setComponent enemyId (ComponentPosition.mapX (\_ -> 5) ComponentPosition.identity)

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
      , positionComponents = positionComponents
      , lifeComponents = lifeComponents
      , visualComponents = visualComponents
      , entityIdDebug = Nothing
      }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float
    | Clicked -- TODO: Move Clicked Msg to VisualComponent
    | DisplayDebug EntityId
    | HideDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( newPositionComponents, newLifeComponents ) =
                    damageSystem dt model.entities ( model.positionComponents, model.lifeComponents )

                finalPositionComponents =
                    moveSystem dt model.entities newPositionComponents
            in
            ( { model
                | entities = model.entities
                , positionComponents = finalPositionComponents
                , lifeComponents = newLifeComponents
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


moveSystem : Float -> EntityTable -> Table ComponentPosition -> Table ComponentPosition
moveSystem dt entityTable positionComponents =
    foldlEntityTable
        (mapTable (updateMoveSystem dt))
        positionComponents
        entityTable


updateMoveSystem : Float -> ComponentPosition -> ComponentPosition
updateMoveSystem dt position =
    ComponentPosition.mapX (\x -> x) position



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
            [ case getComponent model.visualComponents entityId of
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
    Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))



-- Entity Table


type EntityId
    = EntityId Int


type EntityTable
    = EntityTable Int (List EntityId)


emptyEntityTable : EntityTable
emptyEntityTable =
    EntityTable 0 []


addEntity : EntityTable -> ( EntityTable, EntityId )
addEntity (EntityTable nextId entities) =
    ( EntityTable (nextId + 1) (EntityId nextId :: entities)
    , EntityId nextId
    )


foldlEntityTable : (EntityId -> a -> a) -> a -> EntityTable -> a
foldlEntityTable f a (EntityTable _ entities) =
    List.foldl f a entities


mapEntityTable : (EntityId -> a) -> EntityTable -> List a
mapEntityTable f (EntityTable _ entities) =
    List.map f entities



-- Component Table


type Table a
    = Table (Dict Int a)


emptyComponentTable : Table a
emptyComponentTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent (EntityId entityId) component (Table dict) =
    Table (Dict.insert entityId component dict)


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict


mapComponent : (a -> res) -> Table a -> EntityId -> Maybe res
mapComponent f tableA entityId =
    Maybe.map
        f
        (getComponent tableA entityId)


map2Component : (a -> b -> c) -> ( Table a, Table b ) -> EntityId -> Maybe c
map2Component f ( tableA, tableB ) entityId =
    Maybe.map2
        f
        (getComponent tableA entityId)
        (getComponent tableB entityId)


mapTable : (a -> a) -> EntityId -> Table a -> Table a
mapTable f entityId tableA =
    case mapComponent f tableA entityId of
        Just a ->
            setComponent entityId a tableA

        Nothing ->
            tableA


mapTable2 : (a -> b -> ( a, b )) -> EntityId -> ( Table a, Table b ) -> ( Table a, Table b )
mapTable2 f entityId ( tableA, tableB ) =
    case map2Component f ( tableA, tableB ) entityId of
        Just ( a, b ) ->
            ( setComponent entityId a tableA, setComponent entityId b tableB )

        Nothing ->
            ( tableA, tableB )
