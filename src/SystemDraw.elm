module SystemDraw exposing (..)

import ComponentVisual exposing (ComponentVisual)
import EntityTable exposing (EntityId, EntitySet, Table, mapEntities, valuesTable)
import Event exposing (Msg(..))
import Svg exposing (Svg)


visualToSvg :
    Table ComponentVisual
    -> EntitySet
    -> List (Svg Msg)
visualToSvg visualTable entitySet =
    mapEntities (\entityId visual -> toSvg entityId visual) visualTable entitySet
        |> valuesTable


toSvg : EntityId -> ComponentVisual -> Svg Msg
toSvg entityId visual =
    Svg.map
        (\visualMsg ->
            if visualMsg == Event.Clicked then
                Event.DisplayDebug entityId

            else
                DiscardMsg
        )
        (visual.shape
            (visual.attributes ++ visual.posToAttributes visual.position)
            []
        )
