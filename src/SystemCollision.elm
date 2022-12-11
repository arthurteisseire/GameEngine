module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OutputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


updateEntities : EntitySet -> World -> World
updateEntities entitySet world =
    let
        ( entitiesWhoDidntMoved, newWorld ) =
            foldlEntitySet
                (\entityId ( currentEntitiesWhoDidntMoved, currentWorld ) ->
                    let
                        ( thisEntityMoved, newCurrentWorld ) =
                            updateEntity entityId currentWorld

                        newEntitiesWhoDidntMoved =
                            if thisEntityMoved then
                                filterEntities (\id -> entityId /= id) currentEntitiesWhoDidntMoved

                            else
                                currentEntitiesWhoDidntMoved
                    in
                    ( newEntitiesWhoDidntMoved, newCurrentWorld )
                )
                ( entitySet, world )
                entitySet
    in
    if entitiesWhoDidntMoved /= entitySet then
        updateEntities entitiesWhoDidntMoved newWorld

    else
        newWorld


updateEntity : EntityId -> World -> ( Bool, World )
updateEntity entityId world =
    Maybe.withDefault ( False, world ) <|
        Maybe.map2
            (\position velocity ->
                let
                    ( hasMoved, components ) =
                        collide
                            (InputComponents
                                |> using world.entities
                                |> remove entityId
                                |> andFrom world.positionComponents
                                |> andFrom world.velocityComponents
                            )
                            entityId
                            (OutputComponents position velocity)
                in
                ( hasMoved
                , { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                  }
                )
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.velocityComponents)


collide : Table InputComponents -> EntityId -> OutputComponents -> ( Bool, OutputComponents )
collide otherComponents _ components =
    let
        otherPositions =
            mapTable
                (\_ { position, velocity } -> position.currentPos)
                otherComponents

        nextPosition =
            Vector2.add components.position.currentPos components.velocity
    in
    if components.velocity == Vector2.identity || hasValueInTable nextPosition otherPositions then
        ( False
        , components
        )

    else
        ( True
        , { position = ComponentPosition.init nextPosition
          , velocity = components.velocity
          }
        )



-- Simpler but not fully functional. Kept to test performance


updateEntitiesSimple : EntitySet -> World -> World
updateEntitiesSimple entitySet world =
    foldlEntitySet
        updateEntitySimple
        world
        entitySet


updateEntitySimple : EntityId -> World -> World
updateEntitySimple entityId world =
    update2Components
        (collideSimple
            (select InputComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.positionComponents
                |> andFrom world.velocityComponents
            )
        )
        OutputComponents
        positionComponent
        velocityComponent
        entityId
        world


collideSimple : Table InputComponents -> EntityId -> OutputComponents -> OutputComponents
collideSimple otherComponents _ components =
    let
        otherPositions =
            mapTable
                (\_ { position, velocity } -> position.currentPos)
                otherComponents

        nextPosition =
            Vector2.add components.position.currentPos components.velocity
    in
    if components.velocity == Vector2.identity || hasValueInTable nextPosition otherPositions then
        components

    else
        { position = ComponentPosition.init nextPosition
        , velocity = components.velocity
        }
