-- Main.elm


module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style, type_, value)
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


type alias Model =
    { name : String
    , color : Color
    }


init : Model
init =
    { name = "", color = Red }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change name ->
            { model | name = name }



-- VIEW


viewInputName : String -> String -> String -> (String -> Msg) -> Html Msg
viewInputName t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


view : Model -> Html Msg
view model =
    div []
        [ text "Name: "
        , viewInputName "text" "Name: " model.name Change
        , text "Color: "
        , div [ style "color" "red" ] [ text model.name ]
        ]
