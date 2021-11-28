module Main exposing (main)

import Browser
import Browser.Events
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
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
    }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( entities, entityId ) =
            addEntity emptyEntityTable

        positionComponents =
            emptyComponentTable
                |> setComponent entityId ComponentPosition.identity

        lifeComponents =
            emptyComponentTable
                |> setComponent entityId ComponentLife.identity
    in
    ( { entities = entities
      , positionComponents = positionComponents
      , lifeComponents = lifeComponents
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
                    damageSystem model.entities ( model.positionComponents, model.lifeComponents )

                finalPositionComponents =
                    moveSystem model.entities newPositionComponents
            in
            ( { entities = model.entities
              , positionComponents = finalPositionComponents
              , lifeComponents = newLifeComponents
              }
            , Cmd.none
            )


damageSystem : EntityTable -> ( Table ComponentPosition, Table ComponentLife ) -> ( Table ComponentPosition, Table ComponentLife )
damageSystem entityTable componentTables =
    foldlEntityTable
        (mapTable2 updateDamageSystem)
        componentTables
        entityTable


updateDamageSystem : ComponentPosition -> ComponentLife -> ( ComponentPosition, ComponentLife )
updateDamageSystem position life =
    ( position, ComponentLife.mapHp (\hp -> hp - 1) life )


moveSystem : EntityTable -> Table ComponentPosition -> Table ComponentPosition
moveSystem entityTable positionComponents =
    foldlEntityTable
        (mapTable updateMoveSystem)
        positionComponents
        entityTable


updateMoveSystem : ComponentPosition -> ComponentPosition
updateMoveSystem position =
    ComponentPosition.mapX (\x -> x + 1) position



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
            (systemDraw model.entities model.positionComponents)
        ]
    }


systemDraw : EntityTable -> Table ComponentPosition -> List (Svg Msg)
systemDraw entityTable positionComponents =
    List.filterMap
        identity
        (mapEntityTable
            (mapComponent toSvg positionComponents)
            entityTable
        )


toSvg : ComponentPosition -> Svg Msg
toSvg position =
    Svg.rect
        [ SA.x <| String.fromInt (ComponentPosition.getX position)
        , SA.y <| String.fromInt (ComponentPosition.getY position)
        , SA.width "1"
        , SA.height "1"
        , SA.fill "black"
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


map2Component : (a -> b -> ( resA, resB )) -> Table a -> Table b -> EntityId -> Maybe ( resA, resB )
map2Component f tableA tableB entityId =
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
    case map2Component f tableA tableB entityId of
        Just ( a, b ) ->
            ( setComponent entityId a tableA, setComponent entityId b tableB )

        Nothing ->
            ( tableA, tableB )
