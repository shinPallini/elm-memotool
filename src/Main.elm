-- Main.elm


module Main exposing (main)

import Browser
import Debug
import Html exposing (Attribute, Html, button, div, input, label, option, select, text)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List
import MySvg exposing (roundRect)



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
    { addPlayer : Player
    , playerList : List Player
    }


init : Model
init =
    { addPlayer =
        { name = "", color = Red }
    , playerList = []
    }



-- UPDATE


type Msg
    = Name String
    | Color Color
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            let
                player =
                    model.addPlayer

                newAddPlayer =
                    { player | name = name }
            in
            { model | addPlayer = newAddPlayer }

        Color color ->
            let
                player =
                    model.addPlayer

                newAddPlayer =
                    { player | color = color }
            in
            { model | addPlayer = newAddPlayer }

        Submit ->
            let
                playerList =
                    model.playerList

                newPlayerList =
                    List.append playerList [ model.addPlayer ]

                initAddPlayer =
                    { name = "", color = Red }
            in
            { model
                | addPlayer = initAddPlayer
                , playerList = newPlayerList
            }



-- VIEW


items : List Color
items =
    [ Red, Blue, Green, NoMatch ]


viewBuild : Model -> Html Msg
viewBuild model =
    div []
        [ viewForm model
        ]


viewInputName : String -> String -> String -> (String -> Msg) -> Html Msg
viewInputName t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


createOption : Color -> Html msg
createOption color =
    let
        colorString : String
        colorString =
            colorToString color
    in
    option [ value colorString, style "color" (String.toLower colorString) ] [ text colorString ]


viewSubmitButton : Player -> Html Msg
viewSubmitButton addPlayer =
    let
        changeButtonDisabled =
            case addPlayer.name of
                "" ->
                    [ onClick Submit, disabled True ]

                _ ->
                    [ onClick Submit ]
    in
    button changeButtonDisabled [ text "Submit" ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ label [] [ text "Name: ", viewInputName "text" "Name: " model.addPlayer.name Name ]
        , label [] [ text "Color: ", viewSelectColor model Color ]
        , viewSubmitButton model.addPlayer
        ]


viewSelectOption : List Color -> List (Html msg)
viewSelectOption list =
    List.map createOption list


viewSelectColor : Model -> (Color -> Msg) -> Html Msg
viewSelectColor model toMsg =
    select [ onInput (colorFromString >> toMsg), style "color" (colorToString model.addPlayer.color |> String.toLower) ] (viewSelectOption items)


viewDebugLog : Model -> Html msg
viewDebugLog model =
    div [] [ text (Debug.toString model) ]


view : Model -> Html Msg
view model =
    div []
        [ viewBuild model
        , viewDebugLog model
        , roundRect
        ]
