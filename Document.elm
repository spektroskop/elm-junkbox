module Junkbox.Document exposing
    ( concat
    , empty
    , map
    , none
    , placeholder
    )

import Browser
import Html exposing (Html, text)


none : Browser.Document msg
none =
    { title = ""
    , body = []
    }


placeholder : String -> Browser.Document msg
placeholder str =
    { title = str
    , body = [ text str ]
    }


empty : String -> Browser.Document msg
empty str =
    { title = str
    , body = []
    }


map : (a -> b) -> Browser.Document a -> Browser.Document b
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    }


concat : List (Browser.Document a) -> Browser.Document a
concat docs =
    { title = List.map (\{ title } -> title) docs |> String.join " - "
    , body = List.map (\{ body } -> body) docs |> List.concat
    }
