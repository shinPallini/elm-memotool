-- Main.elm


module Main exposing (main)

import Array exposing (Array)
import Browser
import Debug
import Html exposing (Attribute, Html, button, datalist, div, h1, h2, h3, hr, input, label, li, ol, option, p, s, select, table, td, text, th, thead, tr, ul)
import Html.Attributes exposing (disabled, id, list, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
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
    | Pink
    | Orange
    | Yellow
    | Black
    | White
    | Purple
    | Brown
    | Cyan
    | Lime
    | Maroon
    | Rose
    | Banana
    | Gray
    | Tan
    | Coral
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

        Pink ->
            "Pink"

        Orange ->
            "Orange"

        Yellow ->
            "Yellow"

        Black ->
            "Black"

        White ->
            "White"

        Purple ->
            "Purple"

        Brown ->
            "Browun"

        Cyan ->
            "Cyan"

        Lime ->
            "Lime"

        Maroon ->
            "Maroon"

        Rose ->
            "Rose"

        Banana ->
            "Banana"

        Gray ->
            "Gray"

        Tan ->
            "Tan"

        Coral ->
            "Coral"

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

        "Pink" ->
            Pink

        "Orange" ->
            Orange

        "Yellow" ->
            Yellow

        "Black" ->
            Black

        "White" ->
            White

        "Purple" ->
            Purple

        "Browun" ->
            Brown

        "Cyan" ->
            Cyan

        "Lime" ->
            Lime

        "Maroon" ->
            Maroon

        "Rose" ->
            Rose

        "Banana" ->
            Banana

        "Gray" ->
            Gray

        "Tan" ->
            Tan

        "Coral" ->
            Coral

        _ ->
            NoMatch


type alias Player =
    { name : String
    , color : Color
    , dead : Bool
    , popPosition : String
    , stopPosition : String
    , suspect : String
    }


type alias Scene =
    { playerList : List Player
    }


type alias Model =
    { addPlayer : Player
    , currentSceneIndex : Int
    , sceneArray : Array Scene
    }


init : Model
init =
    { addPlayer = initAddPlayer
    , currentSceneIndex = 0
    , sceneArray =
        Array.fromList
            [ { playerList = []
              }
            ]
    }


initAddPlayer : Player
initAddPlayer =
    { name = ""
    , color = Red
    , dead = False
    , popPosition = ""
    , stopPosition = ""
    , suspect = "可能な範囲"
    }



-- UPDATE


type Msg
    = Name String
    | Color Color
    | Submit
    | SceneMsg Index SceneMsg
    | NextScene
    | PrevScene


type alias Index =
    Int


type SceneMsg
    = PopPosition Player Position
    | StopPosition Player Position
    | Dead Player Bool
    | Undo
    | Suspect Player String


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
                firstScene : Scene
                firstScene =
                    case Array.get 0 model.sceneArray of
                        Just scene ->
                            scene

                        Nothing ->
                            { playerList = [] }

                playerList : List Player
                playerList =
                    firstScene.playerList

                newPlayerList : List Player
                newPlayerList =
                    model.addPlayer :: playerList

                newSceneArray : Array Scene
                newSceneArray =
                    Array.set 0 { firstScene | playerList = newPlayerList } model.sceneArray
            in
            { model
                | addPlayer = initAddPlayer
                , sceneArray = newSceneArray
            }

        SceneMsg index sceneMsg ->
            let
                scene : Scene
                scene =
                    case Array.get index model.sceneArray of
                        Just s ->
                            s

                        Nothing ->
                            { playerList = [] }

                newSceneArray : Array Scene
                newSceneArray =
                    Array.set index (updateScene sceneMsg scene) model.sceneArray
            in
            { model | sceneArray = newSceneArray }

        NextScene ->
            { model | currentSceneIndex = model.currentSceneIndex + 1 }

        PrevScene ->
            { model | currentSceneIndex = model.currentSceneIndex - 1 }



-- updateNextScene :


updateScene : SceneMsg -> Scene -> Scene
updateScene msg scene =
    case msg of
        PopPosition player position ->
            { scene | playerList = updatePopPosition player position scene.playerList }

        StopPosition player position ->
            { scene | playerList = updateStopPosition player position scene.playerList }

        Dead player bool ->
            { scene | playerList = updateDead player bool scene.playerList }

        Undo ->
            let
                len : Int
                len =
                    List.length scene.playerList
            in
            { scene | playerList = List.take (len - 1) scene.playerList }

        Suspect player suspect ->
            { scene | playerList = updateSuspect player suspect scene.playerList }


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


updateDead : Player -> Bool -> List Player -> List Player
updateDead player bool playerList =
    List.map (updatePlayerDead player bool) playerList


updatePlayerDead : Player -> Bool -> Player -> Player
updatePlayerDead changePlayer bool inputPlayer =
    if changePlayer.name == inputPlayer.name then
        { inputPlayer | dead = bool }

    else
        inputPlayer


updateSuspect : Player -> String -> List Player -> List Player
updateSuspect player suspect playerList =
    List.map (updatePlayerSuspect player suspect) playerList


updatePlayerSuspect : Player -> String -> Player -> Player
updatePlayerSuspect changePlayer suspect inputPlayer =
    if changePlayer.name == inputPlayer.name then
        { inputPlayer | suspect = suspect }

    else
        inputPlayer



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewBuild model
        , hr [] []
        , viewDebugLog model

        -- , roundRect
        ]


viewDebugLog : Model -> Html msg
viewDebugLog model =
    text <| Debug.toString model


items : List Color
items =
    [ Red
    , Blue
    , Green
    , Pink
    , Orange
    , Yellow
    , Black
    , White
    , Purple
    , Brown
    , Cyan
    , Lime
    , Maroon
    , Rose
    , Banana
    , Gray
    , Tan
    , Coral
    , NoMatch
    ]


viewBuild : Model -> Html Msg
viewBuild model =
    div []
        [ h1 [] [ text "Among us情報記録用メモ" ]
        , viewForm model
        , hr [] []
        , h2 [] [ text "盤面情報" ]
        , viewCurrentScene model
        ]


viewCurrentScene : Model -> Html Msg
viewCurrentScene model =
    div []
        [ viewSceneInfo model
        , viewSceneChange model
        , h3 [] [ text "入力欄" ]
        , viewPlayerTable model
        , hr [] []
        , h3 [] [ text "キルされたプレイヤー" ]
        , viewKilledPlayer model
        , h3 [] [ text "各プレイヤーの情報" ]
        , viewPlayerInfo model
        ]


viewSceneInfo : Model -> Html msg
viewSceneInfo model =
    h3 [] [ text <| "シーン" ++ (String.fromInt <| model.currentSceneIndex + 1) ]


viewSceneChange : Model -> Html Msg
viewSceneChange model =
    div []
        [ prevButton model "前のシーン"
        , nextButton model "次のシーン"
        ]


prevButton : Model -> String -> Html Msg
prevButton model string =
    let
        attr =
            if model.currentSceneIndex <= 0 then
                [ disabled True, onClick PrevScene ]

            else
                [ onClick PrevScene ]
    in
    button attr [ text string ]


nextButton : Model -> String -> Html Msg
nextButton model string =
    let
        attr =
            if model.currentSceneIndex >= 5 then
                [ disabled True, onClick NextScene ]

            else
                [ onClick NextScene ]
    in
    button attr [ text string ]


viewKilledPlayer : Model -> Html msg
viewKilledPlayer model =
    let
        scene =
            getScene model.currentSceneIndex model.sceneArray

        deadPlayers =
            List.filter .dead scene.playerList
    in
    ol [] (List.map viewPlayerName deadPlayers)


getCurrentScene : Model -> Scene
getCurrentScene model =
    case Array.get model.currentSceneIndex model.sceneArray of
        Just scene ->
            scene

        Nothing ->
            { playerList = [] }


getScene : Index -> Array Scene -> Scene
getScene index array =
    case Array.get index array of
        Just scene ->
            scene

        Nothing ->
            { playerList = [] }



-- text <| Debug.toString deadPlayers


viewPlayerName : Player -> Html msg
viewPlayerName player =
    li []
        [ text player.name
        , ul []
            [ li [] [ text <| "湧きポイント : " ++ player.popPosition ]
            , li [] [ text <| "最終視認 : " ++ player.stopPosition ]
            ]
        ]


viewInputName : String -> String -> String -> (String -> Msg) -> Html Msg
viewInputName t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewPlayerInfo : Model -> Html msg
viewPlayerInfo model =
    let
        scene =
            getScene model.currentSceneIndex model.sceneArray
    in
    div [] (wrapPlayerInfo scene)


wrapPlayerInfo : Scene -> List (Html msg)
wrapPlayerInfo scene =
    List.map (playerInfo scene) suspectLabel


playerInfo : Scene -> String -> Html msg
playerInfo scene label =
    let
        suspectList =
            List.filter (suspectCheck label) scene.playerList
                |> playerNameList

        ulList =
            List.map (\s -> li [] [ text s ]) suspectList
    in
    div [] [ p [] [ text label ], ul [] ulList ]


suspectCheck : String -> Player -> Bool
suspectCheck label player =
    if label == player.suspect then
        True

    else
        False


playerNameList : List Player -> List String
playerNameList playerList =
    List.map .name playerList


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
        [ p [] [ label [] [ text "Name: ", viewInputName "text" "Name: " model.addPlayer.name Name ] ]
        , p [] [ label [] [ text "Color: ", viewSelectColor Color ] ]
        , p [] [ viewSubmitButton model.addPlayer ]
        , p [] [ viewUndoButton model ]
        ]


viewSelectOption : List Color -> List (Html msg)
viewSelectOption list =
    List.map createOption list


viewSelectColor : (Color -> Msg) -> Html Msg
viewSelectColor toMsg =
    select [ onInput (colorFromString >> toMsg) ] (viewSelectOption items)


viewUndoButton : Model -> Html Msg
viewUndoButton model =
    let
        scene =
            getScene 0 model.sceneArray

        undoButtonDisabled : List (Attribute Msg)
        undoButtonDisabled =
            case List.length scene.playerList of
                0 ->
                    [ disabled True ]

                _ ->
                    [ onClick (SceneMsg 0 Undo) ]
    in
    button undoButtonDisabled [ text "Undo" ]



-- TABLE


viewPlayerTable : Model -> Html Msg
viewPlayerTable model =
    div []
        [ playerTable model
        , positionDataList
        , popPositionDataList
        , suspectDataList
        ]


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
                        [ text "キルされた" ]
                    , th []
                        [ text "湧き位置" ]
                    , th []
                        [ text "最終位置" ]
                    , th []
                        [ text "容疑者候補" ]
                    ]
                ]
    in
    table [] (header :: playerRow model)


playerRow : Model -> List (Html Msg)
playerRow model =
    let
        scene =
            getCurrentScene model
    in
    List.map (convertRow model.currentSceneIndex) scene.playerList


convertRow : Index -> Player -> Html Msg
convertRow index player =
    tr []
        [ td [] [ text player.name ]
        , td [] [ text (colorToString player.color) ]
        , td [] [ input [ type_ "checkbox", onCheck <| SceneMsg index << Dead player ] [] ]
        , td [] [ input [ list "pop_airship", onInput <| SceneMsg index << PopPosition player ] [] ]
        , td [] [ input [ list "airship", onInput <| SceneMsg index << StopPosition player ] [] ]
        , td [] [ select [ onInput <| SceneMsg index << Suspect player ] suspectOption ]
        ]


airship : List String
airship =
    [ "ミーティング"
    , "宿舎前"
    , "金庫室"
    , "昇降機"
    , "エンジンルーム"
    , "コミュニケーション"
    , "コックピット"
    , "武器庫"
    , "キッチン"
    , "展望デッキ(キッチン)"
    , "メインルーム"
    , "セキュリティ"
    , "展望デッキ(セキュリティ)"
    , "電気室"
    , "シャワー室"
    , "アーカイブ"
    , "ラウンジ"
    , "貨物室"
    , "バイタル"
    ]


popAirship : List String
popAirship =
    [ "宿舎前"
    , "エンジンルーム"
    , "キッチン"
    , "メインルーム"
    , "アーカイブ"
    , "貨物室"
    ]


popPositionDataList : Html msg
popPositionDataList =
    datalist [ id "pop_airship" ] (airshipOption popAirship)


positionDataList : Html msg
positionDataList =
    datalist [ id "airship" ] (airshipOption airship)


airshipOption : List String -> List (Html msg)
airshipOption list =
    List.map (\s -> option [] [ text s ]) list


suspectDataList : Html msg
suspectDataList =
    datalist [ id "suspect" ] suspectOption


suspectLabel : List String
suspectLabel =
    [ "可能な範囲", "確白", "容疑者", "ライン" ]


suspectOption : List (Html msg)
suspectOption =
    List.map (\n -> option [] [ text n ]) suspectLabel
