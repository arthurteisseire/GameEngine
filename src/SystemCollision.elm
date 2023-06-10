module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.ComponentTable as ComponentTable
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet)
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Dict exposing (Dict)
import List.Extra as List
import Vector2 exposing (Vector2)
import World exposing (..)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OtherComponents =
    { position : ComponentPosition
    }


type alias Line =
    { start : Vector2 Float
    , end : Vector2 Float
    }


type alias Intersection =
    Vector2 Float


updateEntities : EntitySet -> World -> World
updateEntities entitySet world =
    let
        table : Table Components
        table =
            ComponentTable.select Components
                |> ComponentTable.from world.positionComponents
                |> ComponentTable.innerJoin world.velocityComponents

        collidedTable : Table Components
        collidedTable =
            collideNew table
    in
    updateComponentsInTable collidedTable world


collideNew : Table Components -> Table Components
collideNew table =
    let
        grouped : List (List EntityId)
        grouped =
            groupEntitiesByDestination table

        filtered : List (List EntityId)
        filtered =
            List.filter (\entities -> not <| List.isEmpty entities) grouped

        collidingEntities : List EntityId
        collidingEntities =
            List.concat filtered
    in
    Table.map
        (\entityId components ->
            if List.member entityId collidingEntities then
                { components | velocity = Vector2.identity }

            else
                components
        )
        table


groupEntitiesByDestination : Table Components -> List (List EntityId)
groupEntitiesByDestination table =
    let
        groupedList : List ( ( EntityId, Components ), List ( EntityId, Components ) )
        groupedList =
            List.gatherWith
                (\( entityId1, components1 ) ( entityId2, components2 ) ->
                    Vector2.eq
                        (Vector2.add components1.position components1.velocity)
                        (Vector2.add components2.position components2.velocity)
                )
                (Table.toList table)
    in
    List.foldl
        (\( ( _, _ ), list ) accList ->
            List.map
                (\( entityId, _ ) ->
                    entityId
                )
                list
                :: accList
        )
        []
        groupedList


detectIntersection : Line -> Line -> Maybe Intersection
detectIntersection line1 line2 =
    let
        vector1 =
            { x = line1.end.x - line1.start.x, y = line1.end.y - line1.start.y }

        vector2 =
            { x = line2.end.x - line2.start.x, y = line2.end.y - line2.start.y }

        crossProduct =
            vector1.x * vector2.y - vector1.y * vector2.x
    in
    if line1.end == line2.end then
        Just line1.end

    else if crossProduct == 0 then
        Nothing

    else
        let
            thirdVector =
                { x = line2.start.x - line1.start.x, y = line2.start.y - line1.start.y }

            t =
                (thirdVector.x * vector2.y - thirdVector.y * vector2.x) / crossProduct

            u =
                (thirdVector.x * vector1.y - thirdVector.y * vector1.x) / crossProduct
        in
        if t >= 0 && t <= 1 && u >= 0 && u <= 1 then
            let
                intersectionX =
                    line1.start.x + t * vector1.x

                intersectionY =
                    line1.start.y + t * vector1.y
            in
            Just { x = intersectionX, y = intersectionY }

        else
            Nothing


updateEntitiesOld : EntitySet -> World -> World
updateEntitiesOld entitySet world =
    collideEachEntityRecursively
        ((Db.select OtherComponents
            |> Db.fromEntities .entities
            |> Db.innerJoin .positionComponents
         )
            world
        )
        entitySet
        world


collideEachEntityRecursively : Table OtherComponents -> EntitySet -> World -> World
collideEachEntityRecursively others entitySet world =
    let
        current =
            (Db.select Components
                |> Db.fromEntities (\_ -> entitySet)
                |> Db.innerJoin .positionComponents
                |> Db.innerJoin .velocityComponents
            )
                world

        updatedComponents =
            collideEachEntity others current
    in
    if updatedComponents == Table.empty then
        world

    else
        updateEntities
            (EntitySet.filter
                (\entityId -> not <| List.member entityId (Table.keys updatedComponents))
                entitySet
            )
            (updateComponentsInTable updatedComponents world)


collideEachEntity : Table OtherComponents -> Table Components -> Table Components
collideEachEntity others current =
    Table.foldl
        (\entityId components table ->
            case collide others components of
                Just updatedComponents ->
                    Table.insert entityId updatedComponents table

                Nothing ->
                    table
        )
        Table.empty
        current


collide : Table OtherComponents -> Components -> Maybe Components
collide otherComponents components =
    let
        otherPositions =
            Table.map (\_ { position } -> position) otherComponents

        nextPosition =
            Vector2.add components.position components.velocity
    in
    if components.velocity == Vector2.identity || Table.hasValue nextPosition otherPositions then
        Nothing

    else
        Just
            { position = nextPosition
            , velocity = components.velocity
            }


updateComponentsInTable : Table Components -> World -> World
updateComponentsInTable table world =
    Table.foldl
        (\entityId components currentWorld ->
            currentWorld
                |> ComponentTable.updateComponent ( ComponentPosition.modifier.map, .position ) components entityId
                |> ComponentTable.updateComponent ( ComponentVelocity.modifier.map, .velocity ) components entityId
        )
        world
        table
