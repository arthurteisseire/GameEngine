module Core.Context exposing (..)

import Core.EntityId exposing (EntityId)


type alias ContextOperations context =
    { toStrings : EntityId -> context -> List String
    , remove : EntityId -> context -> context
    }
