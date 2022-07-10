module SystemAttack exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


--update : EntityTable -> ( Table ComponentLife, Table ComponentVelocity, Table ComponentPosition ) -> ( Table ComponentLife, Table ComponentVelocity, Table ComponentPosition )
--update entityTable ( lifeTable, velocityTable, positionTable ) =
--    let
--        entities =
--            map3ToEntityList entityTable ( lifeTable, velocityTable, positionTable )
--
--        updatedEntities =
--            fight entities
--    in
--    update3ComponentsTablesFromEntityList ( lifeTable, positionTable, velocityTable ) updatedEntities


fight : List ( EntityId, ComponentLife, ComponentPosition ) -> List ( EntityId, ComponentLife, ComponentPosition )
fight entities =
    List.map
        (\entity -> attack entities entity)
        entities


attack : List ( EntityId, ComponentLife, ComponentPosition ) -> ( EntityId, ComponentLife, ComponentPosition ) -> ( EntityId, ComponentLife, ComponentPosition )
attack entities ( id, life, position ) =
    ( id, life, position )
