module SystemLife exposing (..)

import ComponentDamage exposing (ComponentDamage)
import ComponentLife exposing (ComponentLife)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import World exposing (..)


type alias OutputComponents =
    { life : ComponentLife
    }


type alias InputComponents =
    { life : ComponentLife
    , damage : ComponentDamage
    }


updateEntity : EntityId -> World -> World
updateEntity =
    Db.updateComponents
        { func = takeDamage
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentLife.modifier.get
                |> Component.join ComponentDamage.modifier.get
        , output =
            Modifier.select
                |> Modifier.join ( ComponentLife.modifier.map, .life )
        }


takeDamage : InputComponents -> OutputComponents
takeDamage { damage, life } =
    let
        damages =
            List.foldl (\dam sum -> sum + dam.points) 0 damage
    in
    { life = { life | healPoints = life.healPoints - damages }
    }
