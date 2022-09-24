module SystemLife exposing (..)

import ComponentDamage exposing (ComponentDamage)
import ComponentLife exposing (ComponentLife)
import EntityTable exposing (..)
import World exposing (World)


type alias Components =
    { damage : ComponentDamage
    , life : ComponentLife
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map2
            (\damage life ->
                let
                    components =
                        takeDamage entityId (Components damage life)
                in
                { world
                    | damageComponents = insertComponent entityId components.damage world.damageComponents
                    , lifeComponents = insertComponent entityId components.life world.lifeComponents
                }
            )
            (getComponent entityId world.damageComponents)
            (getComponent entityId world.lifeComponents)


takeDamage : EntityId -> Components -> Components
takeDamage _ { damage, life } =
    let
        damages =
            List.foldl (\dam sum -> sum + dam.damage) 0 damage
    in
    { damage = damage
    , life = { life | healPoints = life.healPoints - damages }
    }
