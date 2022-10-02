module SystemCollision exposing (..)

import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import World exposing (World)


type alias InputComponents =
    { position : ComponentPosition
    }


type alias OutputComponents =
    { position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map
            (\position ->
                let
                    inputComponents =
                        (InputComponents
                            |> from world.positionComponents
                        )
                            world.entities
                            |> removeInTable entityId

                    components =
                        collide entityId inputComponents (OutputComponents position)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                }
            )
            (getComponent entityId world.positionComponents)


collide : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
collide _ otherPositions { position } =
    { position =
        if hasValueInTable position.currentPos (mapTable (\_ input -> input.position.currentPos) otherPositions) then
            { currentPos = position.previousPos
            , previousPos = position.previousPos
            }

        else
            position
    }
