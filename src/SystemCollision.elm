module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.EntitySet as EntitySet exposing (EntitySet)
import Core.Modifier
import Core.Table as Table exposing (Table)
import Vector2 exposing (Vector2)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    }


type alias OutputComponents =
    { velocity : ComponentVelocity
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


run entitySet context =
    let
        table : Table Components
        table =
            ComponentTable.select Components
                |> ComponentTable.from context.positionComponents
                |> ComponentTable.innerJoin context.velocityComponents

        collidedTable : Table Components
        collidedTable =
            collideNew table
    in
    updateComponentsInTable collidedTable context


collideNew : Table Components -> Table Components
collideNew table =
    table
        |> applyPass (applyPassSingle detectOrthogonalIntersection)
        |> applyPass (applyPassSingle detectOppositeIntersection)


detectOrthogonalIntersection : Line -> Line -> Bool
detectOrthogonalIntersection line1 line2 =
    Vector2.approxEq line1.end line2.end 0.1


detectOppositeIntersection : Line -> Line -> Bool
detectOppositeIntersection v1 v2 =
    Vector2.approxEq v1.end v2.start 0.1
        && Vector2.approxEq v2.end v1.start 0.1


applyPassSingle : (Line -> Line -> Bool) -> Table Components -> Components -> Vector2 Float
applyPassSingle func table components =
    if
        Table.hasValueIf
            (\line tableComponents ->
                func
                    (componentsToLine tableComponents)
                    line
            )
            (componentsToLine components)
            table
    then
        Vector2.identity

    else
        components.velocity


applyPass : (Table Components -> Components -> Vector2 Float) -> Table Components -> Table Components
applyPass func table =
    Table.map
        (\entityId components ->
            { components
                | velocity =
                    func
                        (Table.remove entityId table)
                        components
            }
        )
        table


componentsToLine : Components -> Line
componentsToLine components =
    { start = components.position
    , end = Vector2.add components.position components.velocity
    }


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


updateEntitiesOld entitySet context =
    collideEachEntityRecursively
        ((Context.select OtherComponents
            |> Context.fromEntities .entities
            |> Context.innerJoin .positionComponents
         )
            context
        )
        entitySet
        context


collideEachEntityRecursively others entitySet context =
    let
        current =
            (Context.select Components
                |> Context.fromEntities (\_ -> entitySet)
                |> Context.innerJoin .positionComponents
                |> Context.innerJoin .velocityComponents
            )
                context

        updatedComponents =
            collideEachEntity others current
    in
    if updatedComponents == Table.empty then
        context

    else
        run
            (EntitySet.filter
                (\entityId -> not <| List.member entityId (Table.keys updatedComponents))
                entitySet
            )
            (updateComponentsInTable updatedComponents context)


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


updateComponentsInTable table context =
    Table.foldl
        (\entityId components currentWorld ->
            currentWorld
                |> ComponentTable.updateComponent ( ComponentPosition.modifier.map, .position ) components entityId
                |> ComponentTable.updateComponent ( ComponentVelocity.modifier.map, .velocity ) components entityId
        )
        context
        table
