module ECS exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { positions : Table PositionComponent
    , lifes : Table LifeComponent
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        entity =
            EntityId 0
    in
    ( { positions =
            emptyComponentTable
                |> setComponent entity (PositionComponent 0)
      , lifes =
            emptyComponentTable
                |> setComponent entity (LifeComponent 100)
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
                    Maybe.map2 damageSystem (getComponent entity model.positions) (getComponent entity model.lifes)
            in
            case maybe of
                Just ( position, life ) ->
                    ( { positions = model.positions |> setComponent entity position
                      , lifes = model.lifes |> setComponent entity life
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


damageSystem : PositionComponent -> LifeComponent -> ( PositionComponent, LifeComponent )
damageSystem position life =
    ( position, life )


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



-- Components


type PositionComponent
    = PositionComponent Int


type LifeComponent
    = LifeComponent Int



-- Table


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
