module SystemLife exposing (..)

import ComponentDamage exposing (ComponentDamage)
import ComponentLife exposing (ComponentLife)
import EntityTable exposing (..)
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
    updateComponents
        { func = takeDamage
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .lifeComponents
                |> withInput .damageComponents
        , output =
            toOutputComponents
                |> withOutput lifeComponent
        }


takeDamage : InputComponents -> OutputComponents
takeDamage { damage, life } =
    let
        damages =
            List.foldl (\dam sum -> sum + dam.points) 0 damage
    in
    { life = { life | healPoints = life.healPoints - damages }
    }
