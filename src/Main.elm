module Main exposing (main)

import Browser
import Browser.Events
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as SA


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
      }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float


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
        [ Svg.svg
            [ SA.width "300"
            , SA.height "300"
            , SA.viewBox "0 0 10 10"
            ]
            (systemDraw model.entities (model.visualComponents, model.positionComponents))
        ]
    }


systemDraw : EntityTable -> ( Table ComponentVisual, Table ComponentPosition ) -> List (Svg Msg)
systemDraw entityTable componentTables =
    List.filterMap
        identity
        (mapEntityTable
            (map2ComponentView toSvg componentTables)
            entityTable
        )


toSvg : ComponentVisual -> ComponentPosition -> Svg Msg
toSvg visual position =
    Svg.rect
        [ SA.x <| String.fromInt (ComponentPosition.getX position)
        , SA.y <| String.fromInt (ComponentPosition.getY position)
        , SA.width "1"
        , SA.height "1"
        , SA.fill (ComponentVisual.getColor visual)
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


foldlEntityTable : (EntityId -> b -> b) -> b -> EntityTable -> b
foldlEntityTable f b (EntityTable _ entities) =
    List.foldl f b entities


mapEntityTable : (EntityId -> b) -> EntityTable -> List b
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


map2ComponentView : (a -> b -> Svg Msg) -> ( Table a, Table b ) -> EntityId -> Maybe (Svg Msg)
map2ComponentView f (tableA, tableB) entityId =
    Maybe.map2
        f
        (getComponent tableA entityId)
        (getComponent tableB entityId)

map2Component : (a -> b -> ( resA, resB )) -> ( Table a, Table b ) -> EntityId -> Maybe ( resA, resB )
map2Component f (tableA, tableB) entityId =
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
    case map2Component f (tableA, tableB) entityId of
        Just ( a, b ) ->
            ( setComponent entityId a tableA, setComponent entityId b tableB )

        Nothing ->
            ( tableA, tableB )
