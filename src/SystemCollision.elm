module SystemCollision exposing (update)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


update : Float -> EntityTable -> ( Table ComponentPosition, Table ComponentVelocity ) -> ( Table ComponentPosition, Table ComponentVelocity )
update dt entityTable ( positionTable, velocityTable ) =
    let
        entitiesWithVelocity =
            map2ToEntityList entityTable ( positionTable, velocityTable )

        allEntities =
            mapToEntityList entityTable positionTable

        updatedEntities =
            collide allEntities entitiesWithVelocity
    in
    updateComponentTables ( positionTable, velocityTable ) updatedEntities


collide : List ( EntityId, ComponentPosition ) -> List ( EntityId, ComponentPosition, ComponentVelocity ) -> List ( EntityId, ComponentPosition, ComponentVelocity )
collide allEntities entitiesWithVelocity =
    List.map
        (\entity -> collideEntity allEntities entity)
        entitiesWithVelocity


collideEntity : List ( EntityId, ComponentPosition ) -> ( EntityId, ComponentPosition, ComponentVelocity ) -> ( EntityId, ComponentPosition, ComponentVelocity )
collideEntity allEntities ( entityId, position, velocity ) =
    let
        movedPos =
            { x = position.x + velocity.x, y = position.y + velocity.y }

        nextPos =
            if List.member movedPos (List.map Tuple.second allEntities) then
                position

            else
                movedPos
    in
    ( entityId
    , nextPos
    , ComponentVelocity.identity
    )


updateComponentTables : ( Table ComponentPosition, Table ComponentVelocity ) -> List ( EntityId, ComponentPosition, ComponentVelocity ) -> ( Table ComponentPosition, Table ComponentVelocity )
updateComponentTables ( positionTable, velocityTable ) entityList =
    let
        newPositionTable =
            List.foldl
                (\( entityId, position, _ ) table -> setComponent entityId position table)
                positionTable
                entityList

        newVelocityTable =
            List.foldl
                (\( entityId, _, velocity ) table -> setComponent entityId velocity table)
                velocityTable
                entityList
    in
    ( newPositionTable, newVelocityTable )


mapToEntityList : EntityTable -> Table a -> List ( EntityId, a )
mapToEntityList entityTable componentTable =
    filterMapEntityTable
        (\id -> getComponent componentTable id)
        entityTable


getComponent : Table a -> EntityId -> Maybe ( EntityId, a )
getComponent componentTable entityId =
    mapComponent (\a -> ( entityId, a )) componentTable entityId


map2ToEntityList : EntityTable -> ( Table a, Table b ) -> List ( EntityId, a, b )
map2ToEntityList entityTable componentTables =
    filterMapEntityTable
        (\id -> get2Components componentTables id)
        entityTable


get2Components : ( Table a, Table b ) -> EntityId -> Maybe ( EntityId, a, b )
get2Components ( positionTable, velocityTable ) entityId =
    map2Component (\a b -> ( entityId, a, b )) ( positionTable, velocityTable ) entityId
