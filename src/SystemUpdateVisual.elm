module SystemUpdateVisual exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (..)
import World exposing (..)


type alias OutputComponents =
    { visual : ComponentVisual
    }


type alias InputComponents =
    { visual : ComponentVisual
    , position : ComponentPosition
    }


toPair : InputComponents -> ( ComponentVisual, ComponentPosition )
toPair inputComponents =
    ( inputComponents.visual, inputComponents.position )



-- TODO: updateEntity point-free


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    updateComponentsNewTest
        { db = world
        , entityId = entityId
        , func = updateVisual
        , inputComponents =
            getComponents InputComponents
                |> andIn world.visualComponents
                |> andIn world.positionComponents
        , output =
            update1ComponentNew
                visualComponent
        }


getComponents :
    (ComponentVisual -> ComponentPosition -> InputComponents)
    -> EntityId
    -> Maybe (ComponentVisual -> ComponentPosition -> InputComponents)
getComponents func _ =
    Just func


andIn : Table a -> (EntityId -> Maybe (a -> b)) -> EntityId -> Maybe b
andIn table nestedFunc entityId =
    Maybe.andThen
        (\func -> getNextComponent table func entityId)
        (nestedFunc entityId)


getNextComponent : Table a -> (a -> b) -> EntityId -> Maybe b
getNextComponent table func entityId =
    Maybe.map
        (\a -> func a)
        (getComponent entityId table)


updateVisual : InputComponents -> OutputComponents
updateVisual { visual, position } =
    { visual =
        { visual
            | position =
                { x = position.currentPos.x
                , y = position.currentPos.y
                }
        }
    }
