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
    update2ComponentsTablesFromEntityList ( positionTable, velocityTable ) updatedEntities


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
