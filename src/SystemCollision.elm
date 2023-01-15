module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
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
        ((select OtherComponents
            |> using .entities
            |> andFrom .positionComponents
         )
            world
        )
        entitySet
        world


collideEachEntityRecursively : Table OtherComponents -> EntitySet -> World -> World
collideEachEntityRecursively others entitySet world =
    let
        current =
            (select Components
                |> using (\_ -> entitySet)
                |> andFrom .positionComponents
                |> andFrom .velocityComponents
            )
                world

        updatedComponents =
            collideEachEntity others current
    in
    if updatedComponents == emptyTable then
        world

    else
        updateEntities
            (filterEntities
                (\entityId -> not <| List.member entityId (keysTable updatedComponents))
                entitySet
            )
            (updateComponentsInTable updatedComponents world)


collideEachEntity : Table OtherComponents -> Table Components -> Table Components
collideEachEntity others current =
    foldlTable
        (\entityId components table ->
            case collide others components of
                Just updatedComponents ->
                    insertComponent entityId updatedComponents table

                Nothing ->
                    table
        )
        emptyTable
        current


collide : Table OtherComponents -> Components -> Maybe Components
collide otherComponents components =
    let
        otherPositions =
            mapTable (\_ { position } -> position) otherComponents

        nextPosition =
            Vector2.add components.position components.velocity
    in
    if components.velocity == Vector2.identity || hasValueInTable nextPosition otherPositions then
        Nothing

    else
        Just
            { position = nextPosition
            , velocity = components.velocity
            }


updateComponentsInTable : Table Components -> World -> World
updateComponentsInTable table world =
    foldlTable
        (\entityId components currentWorld ->
            currentWorld
                |> updateComponentInTable positionComponent components entityId
                |> updateComponentInTable velocityComponent components entityId
        )
        world
        table
