module ECS exposing (main)

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
    { positionComponents : Table ComponentPosition
    , lifeComponents : Table ComponentLife
    }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    let
        entity =
            EntityId 0

        positionComponents =
            emptyComponentTable
                |> setComponent entity ComponentPosition.identity

        lifeComponents =
            emptyComponentTable
                |> setComponent entity ComponentLife.identity
    in
    ( { positionComponents = positionComponents
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
                entity =
                    EntityId 0

                ( newPositionComponents, newLifeComponents ) =
                    mapTable2 damageSystem entity model.positionComponents model.lifeComponents

                newPosComponents =
                    mapTable moveSystem entity newPositionComponents
            in
            ( { positionComponents = newPosComponents
              , lifeComponents = newLifeComponents
              }
            , Cmd.none
            )


damageSystem : ComponentPosition -> ComponentLife -> ( ComponentPosition, ComponentLife )
damageSystem position life =
    ( position, ComponentLife.mapHp (\hp -> hp - 1) life )


moveSystem : ComponentPosition -> ComponentPosition
moveSystem position =
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
            (List.filterMap identity [ mapComponent systemDraw (EntityId 0) model.positionComponents ])
        ]
    }


systemDraw : ComponentPosition -> Svg Msg
systemDraw position =
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



-- Component Table


type Table a
    = Table (Dict Int a)


type EntityId
    = EntityId Int


emptyComponentTable : Table a
emptyComponentTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent (EntityId entityId) component (Table dict) =
    Table (Dict.insert entityId component dict)


getComponent : EntityId -> Table a -> Maybe a
getComponent (EntityId id) (Table dict) =
    Dict.get id dict


mapComponent : (a -> res) -> EntityId -> Table a -> Maybe res
mapComponent f entityId tableA =
    Maybe.map
        f
        (getComponent entityId tableA)


map2Component : (a -> b -> ( resA, resB )) -> EntityId -> Table a -> Table b -> Maybe ( resA, resB )
map2Component f entityId tableA tableB =
    Maybe.map2
        f
        (getComponent entityId tableA)
        (getComponent entityId tableB)


mapTable : (a -> a) -> EntityId -> Table a -> Table a
mapTable f entityId tableA =
    case mapComponent f entityId tableA of
        Just a ->
            setComponent entityId a tableA

        Nothing ->
            tableA


mapTable2 : (a -> b -> ( a, b )) -> EntityId -> Table a -> Table b -> ( Table a, Table b )
mapTable2 f entityId tableA tableB =
    case map2Component f entityId tableA tableB of
        Just ( a, b ) ->
            ( setComponent entityId a tableA, setComponent entityId b tableB )

        Nothing ->
            ( tableA, tableB )
