module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html
import Html.Events


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { entities : EntityTable
    , pause : Bool
    }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { entities =
            emptyEntityTable
                |> addEntityAndDiscard initComponentSet
                |> addEntityAndDiscard exampleComponentSet
      , pause = False
      }
    , Cmd.none
    )




-- Update


type Msg
    = Tick Float
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | entities = Debug.log "Entities" <| fellSystem dt model.entities }
            , Cmd.none
            )

        TogglePause ->
            ( { model | pause = not model.pause }
            , Cmd.none
            )




-- View


view : Model -> Browser.Document Msg
view _ =
    { title = "GameEngine"
    , body =
        [ Html.div
            [ Html.Events.onClick TogglePause
            ]
            [ Html.text "coucou"
            ]
        ]
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        Sub.none
    else
        Browser.Events.onAnimationFrameDelta (\millis -> Tick (millis / 1000))



-- Entity


type alias EntityTable =
    Table ComponentSet


type Table a
    = Table Int (Dict Int a)


type Id
    = Id Int


emptyEntityTable : Table a
emptyEntityTable =
    Table 0 Dict.empty


addEntity : a -> Table a -> ( Table a, Id )
addEntity entity (Table nextId dict) =
    ( Table (nextId + 1) (Dict.insert nextId entity dict)
    , Id nextId
    )

addEntityAndDiscard : a -> Table a -> Table a
addEntityAndDiscard entity (Table nextId dict) =
    Table (nextId + 1) (Dict.insert nextId entity dict)

getEntity : Id -> Table a -> Maybe a
getEntity (Id id) (Table _ dict) =
    Dict.get id dict


mapComponentSet : (a -> a) -> Table a -> Table a
mapComponentSet f (Table size dict) =
    Table size <|
        Dict.map (\_ a -> f a) dict



-- Components


type alias ComponentSet =
    { lifeComponent : Maybe LifeComponent
    , positionComponent : Maybe PositionComponent
    }


type LifeComponent =
    LifeComponent Int

--type alias LifeComponent =
--    { healPoints : Int
--    }


type alias PositionComponent =
    { x : Int
    }


initComponentSet : ComponentSet
initComponentSet =
    { lifeComponent = Nothing
    , positionComponent = Nothing
    }


exampleComponentSet : ComponentSet
exampleComponentSet =
    { lifeComponent = Just (LifeComponent 0)
    , positionComponent = Just { x = 0 }
    }



-- System


--fellSystem : Float -> EntityTable -> EntityTable
--fellSystem dt entityTable =
--    mapComponentSet
--        (\componentSet ->
--            let
--                updatedComponents =
--                     Maybe.map
--                        updateFellComponents
--                        (extractFellSystemComponents componentSet)
--            in
--                case updatedComponents of
--                    Nothing ->
--                        componentSet
--
--                    Just ( life, pos ) ->
--                        { lifeComponent = Just life
--                        , positionComponent = Just pos
--                        }
--        )
--        entityTable
--    -- updateFellComponents


--updateFellComponentSet : ComponentSet -> ComponentSet
--updateFellComponentSet componentSet =
--    case tryUpdateFellSystem componentSet of
--        Nothing ->
--            componentSet
--
--        Just ( life, pos ) ->
--            { lifeComponent = Just life
--            , positionComponent = Just pos
--            }
--
--
--tryUpdateFellSystem : ComponentSet -> Maybe ( LifeComponent, PositionComponent )
--tryUpdateFellSystem componentSet =
--    componentSet
--        |> extractFellSystemComponents
--        |> Maybe.map updateFellComponents
--
--
--extractFellSystemComponents : ComponentSet -> Maybe ( LifeComponent, PositionComponent )
--extractFellSystemComponents componentSet =
--    Maybe.map2
--        Tuple.pair
--        componentSet.lifeComponent
--        componentSet.positionComponent



type alias Type a =
    { get : ComponentSet -> Maybe a
    , set : a -> ComponentSet -> ComponentSet
    }

lifeComponentType : Type LifeComponent
lifeComponentType =
    { get = (\set -> set.lifeComponent)
    , set = (\life set -> { set | lifeComponent = Just life })
    }

positionComponentType : Type PositionComponent
positionComponentType =
    { get = (\set -> set.positionComponent)
    , set = (\pos set -> { set | positionComponent = Just pos })
    }


mapType : (a -> a) -> Type a -> ComponentSet -> ComponentSet
mapType f typeA set =
    let
        maybeA =
            Maybe.map
                f
                (typeA.get set)
    in
        case maybeA of
            Just a ->
                typeA.set a set

            Nothing ->
                set

map2Type : (a -> b -> (a, b)) -> (Type a, Type b) -> ComponentSet -> ComponentSet
map2Type f (typeA, typeB) set =
    let
        maybeAB =
            Maybe.map2
                f
                (typeA.get set)
                (typeB.get set)
    in
        case maybeAB of
            Just (a, b) ->
                set
                    |> typeA.set a
                    |> typeB.set b

            Nothing ->
                set


mapMatchingComponents
    : (Type a, Type b)
    -> (a -> b -> (a, b))
    -> EntityTable
    -> EntityTable

mapMatchingComponents components f table =
    mapComponentSet
        (\ componentSet ->
            map2Type
                f
                components
                componentSet
        )
        table


fellSystem : Float -> EntityTable -> EntityTable
fellSystem dt entityTable =
    mapMatchingComponents (lifeComponentType, positionComponentType) updateFellComponents entityTable


updateFellComponents : LifeComponent -> PositionComponent -> ( LifeComponent, PositionComponent )
updateFellComponents (LifeComponent hp) position =
    ( LifeComponent ( hp - 1 )
    , { x = position.x - 1 }
    )


