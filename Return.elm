module Junkbox.Return exposing
    ( Return
    , addCmd
    , andThen
    , map
    , mapBoth
    , mapCmd
    , mapModel
    , noCmd
    , withCmd
    , withModel
    )


type alias Return msg model =
    ( model, Cmd msg )


withCmd : Cmd msg -> model -> Return msg model
withCmd cmd model =
    ( model, cmd )


withModel : model -> Cmd msg -> Return msg model
withModel model cmd =
    ( model, cmd )


noCmd : model -> Return msg model
noCmd model =
    ( model, Cmd.none )


map : (model -> a) -> Return msg model -> Return msg a
map =
    mapModel


mapModel : (model -> a) -> Return msg model -> Return msg a
mapModel f ( model, cmd ) =
    ( f model, cmd )


mapCmd : (msg -> a) -> Return msg model -> Return a model
mapCmd f ( model, cmd ) =
    ( model, Cmd.map f cmd )


mapBoth :
    (model1 -> model2)
    -> (msg1 -> msg2)
    -> Return msg1 model1
    -> Return msg2 model2
mapBoth mf cf ( model, cmd ) =
    ( mf model, Cmd.map cf cmd )


addCmd : Cmd msg -> Return msg model -> Return msg model
addCmd add ( model, cmd ) =
    ( model, Cmd.batch [ add, cmd ] )


andThen : (model -> Return msg a) -> Return msg model -> Return msg a
andThen f ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            f model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )
