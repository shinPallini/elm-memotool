module MySvg exposing (..)

import Html exposing (Html)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (..)


roundRect : Html msg
roundRect =
    svg
        [ width "120", height "120", viewBox "0 0 120 120" ]
        -- color code "#eee" == "#eeeeee"
        [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#FFA500" ] [] ]
