-- Main.elm


module Main exposing (main)

import Browser
import Debug
import Html exposing (Attribute, Html, button, div, input, label, option, select, table, td, text, th, thead, tr)
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
    , popPosition : String
    , stopPosition : String
    }


type alias Model =
    { addPlayer : Player
    , playerList : List Player
    }


init : Model
init =
    { addPlayer =
        { name = ""
        , color = Red
        , popPosition = ""
        , stopPosition = ""
        }
    , playerList = []
    }



-- UPDATE


type Msg
    = Name String
    | Color Color
    | Submit
    | PopPosition Player Position
    | StopPosition Player Position


type alias Position =
    String


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
                    { name = ""
                    , color = Red
                    , popPosition = ""
                    , stopPosition = ""
                    }
            in
            { model
                | addPlayer = initAddPlayer
                , playerList = newPlayerList
            }

        PopPosition player position ->
            { model | playerList = updatePopPosition player position model.playerList }

        StopPosition player position ->
            { model | playerList = updateStopPosition player position model.playerList }


updatePopPosition : Player -> Position -> List Player -> List Player
updatePopPosition player position playerList =
    List.map (updatePlayerPopPosition player position) playerList


updatePlayerPopPosition : Player -> Position -> Player -> Player
updatePlayerPopPosition changePlayer position inputPlayer =
    if changePlayer.name == inputPlayer.name then
        { inputPlayer | popPosition = position }

    else
        inputPlayer


updateStopPosition : Player -> Position -> List Player -> List Player
updateStopPosition player position playerList =
    List.map (updatePlayerStopPosition player position) playerList


updatePlayerStopPosition : Player -> Position -> Player -> Player
updatePlayerStopPosition changePlayer position inputPlayer =
    if changePlayer.name == inputPlayer.name then
        { inputPlayer | stopPosition = position }

    else
        inputPlayer



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewBuild model
        , viewDebugLog model
        , viewPlayerTable model
        , roundRect
        ]


viewDebugLog : Model -> Html msg
viewDebugLog model =
    text <| Debug.toString model


items : List Color
items =
    [ Red, Blue, Green ]


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
        , label [] [ text "Color: ", viewSelectColor Color ]
        , viewSubmitButton model.addPlayer
        ]


viewSelectOption : List Color -> List (Html msg)
viewSelectOption list =
    List.map createOption list


viewSelectColor : (Color -> Msg) -> Html Msg
viewSelectColor toMsg =
    select [ onInput (colorFromString >> toMsg) ] (viewSelectOption items)



-- TABLE


viewPlayerTable : Model -> Html Msg
viewPlayerTable model =
    playerTable model


playerTable : Model -> Html Msg
playerTable model =
    let
        header =
            thead []
                [ tr []
                    [ th []
                        [ text "プレイヤー名" ]
                    , th []
                        [ text "キャラ色" ]
                    , th []
                        [ text "湧き位置" ]
                    , th []
                        [ text "最終位置" ]
                    ]
                ]
    in
    table [] (header :: playerRow model)


playerRow : Model -> List (Html Msg)
playerRow model =
    List.map convertRow model.playerList


convertRow : Player -> Html Msg
convertRow player =
    tr []
        [ td [] [ text player.name ]
        , td [] [ text (colorToString player.color) ]
        , td [] [ input [ onInput (PopPosition player) ] [] ]
        , td [] [ input [ onInput (StopPosition player) ] [] ]
        ]



-- positionInputForm : Html Msg
