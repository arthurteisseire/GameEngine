module SystemTurn exposing (..)

import ComponentTurn exposing (ComponentTurn)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import World exposing (..)


type alias Components =
    { turn : ComponentTurn
    }


updateEntity : EntityId -> World -> World
updateEntity =
    Db.updateComponents
        { func = playTurn
        , inputComponents =
            Component.select Components
                |> Component.join .turnComponents
        , output =
            Modifier.select
                |> Modifier.join turnComponent
        }


playTurn : Components -> Components
playTurn { turn } =
    if turn.remainingTurns == 0 then
        { turn =
            { turn | remainingTurns = turn.turnsToPlay }
        }

    else
        { turn =
            { turn | remainingTurns = turn.remainingTurns - 1 }
        }
