module SystemDraw exposing (..)

import ComponentVisual exposing (ComponentVisual)
import Core.ComponentTable exposing (ComponentTable)
import Core.Context as Context
import Core.EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet)
import Core.Table as Table
import Svg exposing (Svg)


visualToSvg : (EntityId -> ComponentVisual.VisualMsg -> msg) -> ComponentTable ComponentVisual -> EntitySet -> List (Svg msg)
visualToSvg toMsg visualTable entitySet =
    Context.mapEntitiesInTable (\entityId visual -> toSvg toMsg entityId visual) visualTable entitySet
        |> Table.values


toSvg : (EntityId -> ComponentVisual.VisualMsg -> msg) -> EntityId -> ComponentVisual -> Svg msg
toSvg toMsg entityId visual =
    Svg.map
        (toMsg entityId)
        (visual.shape
            (visual.attributes ++ visual.posToAttributes visual.position)
            []
        )
