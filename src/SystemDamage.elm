module SystemDamage exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentLife exposing (ComponentLife)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import World exposing (World)


type alias OutputComponents =
    { position : ComponentPosition
    , life : ComponentLife
    }


updateWorld : World -> World
updateWorld world =
    updateEntitiesWithOthers
        { updateComponents = takeDamage
        , updateWorld =
            \entityId { position, life } accWorld ->
                { accWorld
                    | positionComponents = setComponent entityId position accWorld.positionComponents
                    , lifeComponents = setComponent entityId life accWorld.lifeComponents
                }
        , world = world
        , entityTable = world.entities
        , readTable = mapEntities1 (\_ attack -> attack) world.entities world.attackComponents
        , writeTable = intersectTable2 OutputComponents world.entities world.positionComponents world.lifeComponents
        }


takeDamage : EntityId -> Table ComponentAttack -> OutputComponents -> OutputComponents
takeDamage _ attackTable { position, life } =
    let
        updatedLife =
            if List.member position (List.filterMap (\value -> value) (valuesTable attackTable)) then
                { life | healPoints = life.healPoints - 1 }

            else
                life
    in
    { position = position
    , life = updatedLife
    }
