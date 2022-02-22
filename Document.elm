module Junkbox.Document exposing
    ( empty
    , map
    , merge
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


merge : String -> Browser.Document a -> Browser.Document a -> Browser.Document a
merge sep doc1 doc2 =
    { title = String.join sep [ doc2.title, doc1.title ]
    , body = List.concat [ doc1.body, doc2.body ]
    }
