module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (class)


main =
    view


view =
    div [ class "measure center" ]
        [ div [ class "code f1" ]
            ([ "#..."
             , ".#.e"
             , "e..."
             , "...3"
             ]
                |> List.map (\s -> div [] [ text s ])
            )
        ]
