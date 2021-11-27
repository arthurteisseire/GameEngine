module Main2 exposing (..)


import Dict exposing (Dict)


-- ComponentSetNew


type ComponentSetNew
    = ComponentSetNew (Dict Int Component)


initComponentSetNew : ComponentSetNew
initComponentSetNew =
    ComponentSetNew <| Dict.empty

type Component
    = LifeComp LifeComponent
    | PositionComp PositionComponent



getComponent : Int -> ComponentSetNew -> Maybe Component
getComponent idx (ComponentSetNew set) =
    Dict.get idx set


setLifeComponent : LifeComponent -> ComponentSetNew -> ComponentSetNew
setLifeComponent lifeComponent (ComponentSetNew set) =
    ComponentSetNew <|
        Dict.insert 0 (LifeComp lifeComponent) set


getLifeComponent : ComponentSetNew -> Maybe LifeComponent
getLifeComponent (ComponentSetNew set) =
    case Dict.get 0 set of
        Just (LifeComp lifeComponent) ->
            Just lifeComponent

        _ ->
            Nothing


setPositionComponent : PositionComponent -> ComponentSetNew -> ComponentSetNew
setPositionComponent positionComponent (ComponentSetNew set) =
    ComponentSetNew <|
        Dict.insert 1 (PositionComp positionComponent) set


getPositionComponent : ComponentSetNew -> Maybe PositionComponent
getPositionComponent (ComponentSetNew set) =
    case Dict.get 1 set of
        Just (PositionComp positionComponent) ->
            Just positionComponent

        _ ->
            Nothing

mapLifeComponent : (LifeComponent -> LifeComponent) -> ComponentSetNew -> ComponentSetNew
mapLifeComponent f setNew =
    case getLifeComponent setNew of
        Just lifeComponent ->
            setLifeComponent (f lifeComponent) setNew

        Nothing ->
            setNew


map2Component
    : (Int, Int)
    -> (LifeComponent -> PositionComponent -> (LifeComponent, PositionComponent))
    -> ComponentSetNew
    -> ComponentSetNew
map2Component f setNew =
    case getLifeComponent setNew of
        Just lifeComponent ->
            case getPositionComponent setNew of
                Just positionComponent ->
                    let
                        (life, pos) = f lifeComponent positionComponent
                    in
                        setNew
                            |> setLifeComponent life
                            |> setPositionComponent pos

                Nothing ->
                    setNew

        Nothing ->
            setNew
