module ComponentDamage exposing (..)

import EntityTable exposing (EntityId)


type alias ComponentDamage =
    List
        { fromEntity : EntityId
        , damage : Int
        }


identity : ComponentDamage
identity =
    []
