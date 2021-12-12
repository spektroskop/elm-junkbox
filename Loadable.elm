module Junkbox.Loadable exposing
    ( Loadable(..)
    , State(..)
    , andThen
    , fail
    , fromResult
    , fromTask
    , isFailed
    , isLoaded
    , isLoading
    , isLoadingSlowly
    , loadingSlowly
    , map
    , mapResult
    , reload
    , succeed
    , toMaybe
    , withDefault
    )

import Task exposing (Task)


type State
    = Resolved
    | Reloading


type Loadable e a
    = Initial
    | Loading
    | LoadingSlowly
    | Loaded State a
    | Failed State e


isLoading : Loadable e a -> Bool
isLoading loadable =
    case loadable of
        Loading ->
            True

        LoadingSlowly ->
            True

        Loaded Reloading _ ->
            True

        Failed Reloading _ ->
            True

        _ ->
            False


isLoadingSlowly : Loadable e a -> Bool
isLoadingSlowly loadable =
    case loadable of
        LoadingSlowly ->
            True

        _ ->
            False


isLoaded : Loadable e a -> Bool
isLoaded =
    toMaybe >> Maybe.map (always True) >> Maybe.withDefault False


isFailed : Loadable e a -> Bool
isFailed loadable =
    case loadable of
        Failed _ _ ->
            True

        _ ->
            False


succeed : a -> Loadable e a
succeed =
    Loaded Resolved


fail : e -> Loadable e a
fail =
    Failed Resolved


fromResult : Result e a -> Loadable e a
fromResult result =
    case result of
        Ok value ->
            Loaded Resolved value

        Err error ->
            Failed Resolved error


fromTask : Task e a -> Task x (Loadable e a)
fromTask =
    Task.map (Loaded Resolved)
        >> Task.onError (Failed Resolved >> Task.succeed)


loadingSlowly : Loadable e a -> Loadable e a
loadingSlowly loadable =
    case loadable of
        Loading ->
            LoadingSlowly

        other ->
            other


reload : Loadable e a -> Loadable e a
reload loadable =
    case loadable of
        Initial ->
            Loading

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        Loaded _ v ->
            Loaded Reloading v

        Failed _ e ->
            Failed Reloading e


andThen : (State -> a -> Loadable e b) -> Loadable e a -> Loadable e b
andThen f loadable =
    case loadable of
        Initial ->
            Initial

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        Loaded state a ->
            f state a

        Failed state e ->
            Failed state e


map : (a -> b) -> Loadable e a -> Loadable e b
map f loadable =
    case loadable of
        Initial ->
            Initial

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        Loaded state a ->
            Loaded state (f a)

        Failed state e ->
            Failed state e


withDefault : a -> Loadable e a -> a
withDefault default loadable =
    case loadable of
        Loaded _ a ->
            a

        _ ->
            default


toMaybe : Loadable e a -> Maybe a
toMaybe loadable =
    case loadable of
        Loaded _ a ->
            Just a

        _ ->
            Nothing


mapResult : (a -> Result e v) -> Loadable e a -> Loadable e v
mapResult f loadable =
    case loadable of
        Initial ->
            Initial

        Loading ->
            Loading

        LoadingSlowly ->
            LoadingSlowly

        Failed state error ->
            Failed state error

        Loaded state value ->
            case f value of
                Ok v ->
                    Loaded state v

                Err error ->
                    Failed state error
