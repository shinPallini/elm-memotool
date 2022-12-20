-- Main.elm


module Main exposing (main)

import Browser
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Color
    = Red
    | Blue
    | Green
    | NoMatch


colorToString : Color -> String
colorToString color =
    case color of
        Red ->
            "Red"

        Blue ->
            "Blue"

        Green ->
            "Green"

        NoMatch ->
            "NoMatch"


colorFromString : String -> Color
colorFromString string =
    case string of
        "Red" ->
            Red

        "Blue" ->
            Blue

        "Green" ->
            Green

        _ ->
            NoMatch


type alias Player =
    { name : String
    , color : Color
    }


type alias Model =
    Player


init : Model
init =
    { name = "", color = NoMatch }



-- UPDATE


type Msg
    = Name String
    | Color Color


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Color color ->
            { model | color = color }



-- VIEW


items : List Color
items =
    [ Red, Blue, Green, NoMatch ]


viewBuild : Model -> Html Msg
viewBuild model =
    div []
        [ text "Name: "
        , viewInputName "text" "Name: " model.name Name
        , text "Color: "
        , viewSelectColor Color
        ]


viewInputName : String -> String -> String -> (String -> Msg) -> Html Msg
viewInputName t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


createOption : Color -> Html msg
createOption color =
    let
        colorString =
            colorToString color
    in
    option [ value colorString ] [ text colorString ]


viewSelectOption : List Color -> List (Html msg)
viewSelectOption list =
    List.map createOption list


viewSelectColor : (Color -> Msg) -> Html Msg
viewSelectColor toMsg =
    select [ onInput (colorFromString >> toMsg) ] (viewSelectOption items)


view : Model -> Html Msg
view model =
    div []
        [ viewBuild model
        , div [] [ text ("Name: " ++ model.name) ]
        , div [] [ text ("Color: " ++ colorToString model.color) ]
        ]
