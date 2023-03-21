module SystemDraw exposing (..)

import ComponentVisual exposing (ComponentVisual)
import Core.ComponentTable exposing (ComponentTable)
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet)
import Core.Table as Table
import Event exposing (Msg(..))
import Svg exposing (Svg)


visualToSvg : ComponentTable ComponentVisual -> EntitySet -> List (Svg Msg)
visualToSvg visualTable entitySet =
    Db.mapEntitiesInTable (\entityId visual -> toSvg entityId visual) visualTable entitySet
        |> Table.values


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
