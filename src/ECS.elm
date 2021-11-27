module ECS exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)

import ComponentPosition exposing (ComponentPosition)
import ComponentLife exposing (ComponentLife)


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


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                entity =
                    EntityId 0

                maybe =
                    map2Component damageSystem entity model.positionComponents model.lifeComponents
            in
            case maybe of
                Just ( position, life ) ->
                    ( { positionComponents = model.positionComponents |> setComponent entity position
                      , lifeComponents = model.lifeComponents |> setComponent entity life
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


damageSystem : ComponentPosition -> ComponentLife -> ( ComponentPosition, ComponentLife )
damageSystem position life =
    ( position, ComponentLife.mapHp (\hp -> hp - 1) life )


view : Model -> Browser.Document Msg
view model =
    { title = "ECS"
    , body =
        [ Html.div
            []
            [ Html.text "coucou" ]
        ]
    }


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

map2Component : (a -> b -> (a, b)) -> EntityId -> Table a -> Table b -> Maybe (a, b)
map2Component f entityId tableA tableB =
    Maybe.map2
        f
        (getComponent entityId tableA)
        (getComponent entityId tableB)

