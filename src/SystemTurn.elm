module SystemTurn exposing (..)

import ComponentTurn exposing (ComponentTurn)
import EntityTable exposing (..)
import World exposing (..)


type alias Components =
    { turn : ComponentTurn
    }


updateEntity : EntityId -> World -> World
updateEntity =
    updateComponents
        { func = playTurn
        , inputComponents =
            toInputComponents Components
                |> withInput .turnComponents
        , output =
            toOutputComponents
                |> withOutput turnComponent
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
