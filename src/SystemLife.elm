module SystemLife exposing (..)

import ComponentDamage exposing (ComponentDamage)
import ComponentLife exposing (ComponentLife)
import EntityTable exposing (..)
import World exposing (..)


type alias Components =
    { damage : ComponentDamage
    , life : ComponentLife
    }


updateEntity : EntityId -> World -> World
updateEntity =
    update2Components takeDamage
        Components
        damageComponent
        lifeComponent


takeDamage : EntityId -> Components -> Components
takeDamage _ { damage, life } =
    let
        damages =
            List.foldl (\dam sum -> sum + dam.points) 0 damage
    in
    { damage = damage
    , life = { life | healPoints = life.healPoints - damages }
    }
