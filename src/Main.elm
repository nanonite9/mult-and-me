-- THIS MODULE IS AN APPLICATION TO TEACH STUDENTS MULTIPLICATION
-- KAEL, MEL, AYANA, JUSTIN 

module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List exposing (repeat)
import String
import Dict exposing (Dict)
import Html exposing (Html, Attribute, div, input, text, h1, h3, h4, img, ul, li, node, footer, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Browser
import Tuple

main =
    Browser.sandbox { init = init, update = update, view = view }

-- This is a model we have defined. It basically stores the state of our application.
-- We have 3 values: m = The first number to multiply, n = The second number to multiply, answer = The user's answer to the practice question
type alias Model =
    {
        m: String,
        n: String,
        answer: String
    }

-- This is the initial state of our Model    
init: Model
init = 
    { 
        m = "1",
        n = "1",
        answer = "" 
    }

-- This is defining the actions associated with our state. The m, n, and answer variables can change
type Msg
    = 
    ChangeM String |
    ChangeN String | 
    ChangeAnswer String

-- This is the update portion of our application. Essentially, this is like a reducer!
-- It takes the previous state and updates it to the new value it is being changed to
update: Msg -> Model -> Model
update msg model =
    case msg of 
        ChangeM updateM ->
            { model | m = updateM }
        ChangeN updateN ->
            { model | n = updateN }
        ChangeAnswer updateAnswer ->
            { model | answer = updateAnswer }

-- This function accepts a Model and returns an Html Msg
-- This function displays all of our HTML
view: Model -> Html Msg
view model = 
    div [style "font-family" "Jura",
        style "display" "flex",
        style "justify-content" "center",
        style "flex-direction" "column",
        style "align-items" "center"
        ]
        [ 
        node "link" [ href "https://fonts.googleapis.com/css?family=Jura", rel "stylesheet" ] []
        ,h1 [] [text "Multiplication&Me"]
        ,h3 [] [text "What number do you want to multiply?"]  
        ,h4 [] [text "First number to multiply: "]  
        , input [ placeholder "Enter a number!", value model.m, onInput ChangeM, style "text-align" "center", style "margin-bottom" "16px"] []
        ,h4 [] [text "Second number to multiply: "]   
        , input [ placeholder "Enter a number!", value model.n, onInput ChangeN, style "text-align" "center", style "margin-bottom" "16px" ] []
        ,h3 [] [text <| "What we're calculating: "++model.m++" * "++model.n] 
        , renderList (repeat (Maybe.withDefault 0 (String.toInt model.m)) "") model.n
        , h1 [] [ text "Result" ]
        , img [ src "https://img.freepik.com/free-vector/start_53876-25533.jpg?size=338&ext=jpg", width 300, height 300] []
        , h1 [] [ text  <| String.fromInt (Maybe.withDefault 0 (String.toInt model.m) * Maybe.withDefault 0 (String.toInt model.n)) ]
        , h1 [] [ text "Practice Questions" ]
        , h3 [style "margin-bottom" "32px"] [ text "A rocket explodes into 5 stars. How many stars would there be if 7 rockets explode?" ]
        , renderPracticeQ (repeat 5 "") "7"
        ,h4 [] [text "Answer to the question: "]   
        , input [ placeholder "Enter a number!", value model.answer, onInput ChangeAnswer, style "text-align" "center" ] []
        , renderResult model.answer,
        footer [
            style "background-color" "#263238",
            style "width" "100%",
            style "height" "60px",
            style "color" "white",
            style "text-align" "center"
            ] [p [] [text "Made with 💗 by Mel, Justin, Kael and Ayana"]]
        ]

-- This function accepts a list of strings and a string, it will return an Html Msg
-- The purpose of this function is to render the appropriate amount of rockets to the screen
renderList : List String -> String -> Html msg
renderList lst n =
    div [style "display" "flex", style "flex-wrap" "wrap", style "align-items" "center", style "justify-content" "center"]
        (List.map (\l -> div [style "text-align" "center"]
            [img [ src "http://images.clipartpanda.com/square-clip-art-square-clip-art-8.png", width 300, height 300] []
            , h1 [] [ text n]
            ]) lst)

-- This function accepts a list of strings and a string, it will return an Html Msg
-- The purpose of this function is to render the appropriate amount of things to the screen for the practice question
renderPracticeQ : List String -> String -> Html msg
renderPracticeQ lst n =
    div [style "display" "flex", style "flex-wrap" "wrap", style "align-items" "center", style "justify-content" "center"]
        (List.map (\l -> div [style "text-align" "center"]
            [img [ src "https://www.pinclipart.com/picdir/big/2-22190_michigans-cartoon-rocket-clipart.png", width 400, height 300] []
            , h1 [] [ text n]
            ]) lst)

-- This function accepts a string, it will return an Html Msg
-- The purpose of this function is to display the appropriate message based on the user's input
renderResult : String -> Html msg
renderResult answer =
    if Maybe.withDefault 0 (String.toInt answer) == 35 then
        h1 [] [ text "Wow! Correct!"] 
    else if answer == "" then
        h1 [] [ text ""] 
    else h1 [] [ text "Incorrect! Try again" ]
