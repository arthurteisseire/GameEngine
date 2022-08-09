module SystemDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntityWithOthers2
                takeDamage
                world.entities
                world.attackComponents
                world.positionComponents
                world.lifeComponents
    in
    { world
        | positionComponents = tables.first
        , lifeComponents = tables.second
    }


takeDamage :
    EntityId
    -> Table ComponentAttack
    -> ComponentPosition
    -> ComponentLife
    -> Tuple2 ComponentPosition ComponentLife
takeDamage _ attackTable position life =
    if List.member position (List.filterMap (\value -> value) (valuesTable attackTable)) then
        toTuple2 position { life | healPoints = life.healPoints - 1 }

    else
        toTuple2 position life
