module SystemTurn exposing (..)

import ComponentTurn exposing (ComponentTurn)
import EntityTable exposing (..)
import World exposing (..)


type alias Components =
    { turn : ComponentTurn
    }


updateEntity : EntityId -> World -> World
updateEntity =
    update1Component playTurn
        Components
        turnComponent


playTurn : EntityId -> Components -> Components
playTurn _ { turn } =
    if turn.remainingTurns == 0 then
        { turn =
            { turn | remainingTurns = turn.turnsToPlay }
        }

    else
        { turn =
            { turn | remainingTurns = turn.remainingTurns - 1 }
        }
