module SystemLife exposing (..)

import ComponentDamage exposing (ComponentDamage)
import ComponentLife exposing (ComponentLife)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier


type alias OutputComponents =
    { life : ComponentLife
    }


type alias InputComponents =
    { life : ComponentLife
    , damage : ComponentDamage
    }


updateEntity =
    Context.updateComponents
        { func = takeDamage
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentLife.modifier.get
                |> Component.join ComponentDamage.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentLife.modifier.map, .life )
        }


takeDamage : InputComponents -> OutputComponents
takeDamage { damage, life } =
    let
        damages =
            List.foldl (\dam sum -> sum + dam.points) 0 damage
    in
    { life = { life | healPoints = life.healPoints - damages }
    }
