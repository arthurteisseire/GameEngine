module Core.Context exposing (..)

import Core.EntityId exposing (EntityId)
import Html exposing (Html)


type alias ContextSystem context msg =
    { update : msg -> context -> ( context, Cmd msg )
    , view : context -> Html msg
    , subscriptions : context -> Sub msg
    }


type alias ContextOperations context =
    { toStrings : EntityId -> context -> List String
    , remove : EntityId -> context -> context
    }
