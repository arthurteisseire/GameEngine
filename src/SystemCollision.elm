module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Database as Db
import Core.EntitySet as EntitySet exposing (EntitySet)
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Vector2
import World exposing (..)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OtherComponents =
    { position : ComponentPosition
    }


updateEntities : EntitySet -> World -> World
updateEntities entitySet world =
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
                |> Modifier.updateComponentInTable ( ComponentPosition.modifier.map, .position ) components entityId
                |> Modifier.updateComponentInTable ( ComponentVelocity.modifier.map, .velocity ) components entityId
        )
        world
        table
