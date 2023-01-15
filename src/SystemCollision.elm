module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias OutputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OtherComponents =
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
                            ((InputComponents
                                |> using .entities
                                |> andFrom .positionComponents
                                |> andFrom .velocityComponents
                             )
                                world
                            )
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


collide : Table InputComponents -> OutputComponents -> ( Bool, OutputComponents )
collide otherComponents components =
    let
        otherPositions =
            mapTable
                (\_ { position, velocity } -> position)
                otherComponents

        nextPosition =
            Vector2.add components.position components.velocity
    in
    if components.velocity == Vector2.identity || hasValueInTable nextPosition otherPositions then
        ( False
        , components
        )

    else
        ( True
        , { position = nextPosition
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
    updateComponentsWithOthers
        { func = collideSimple
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .positionComponents
                |> withInput .velocityComponents
        , otherComponents =
            select OtherComponents
                |> using .entities
                |> andFrom .positionComponents
                |> andFrom .velocityComponents
        , output =
            toOutputComponents
                |> withOutput positionComponent
                |> withOutput velocityComponent
        }
        entityId
        world


collideSimple : Table OtherComponents -> InputComponents -> OutputComponents
collideSimple otherComponents components =
    let
        otherPositions =
            mapTable
                (\_ { position, velocity } -> position)
                otherComponents

        nextPosition =
            Vector2.add components.position components.velocity
    in
    if components.velocity == Vector2.identity || hasValueInTable nextPosition otherPositions then
        components

    else
        { position = nextPosition
        , velocity = components.velocity
        }
