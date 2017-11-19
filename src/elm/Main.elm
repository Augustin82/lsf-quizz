module Main exposing (..)

import Html
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Keyed as EK
import Style exposing (..)
import Dict exposing (Dict)
import Random exposing (Seed)
import Time exposing (Time)
import Task
import Random
import Random.List
import Tuple


type Msg
    = NoOp
    | Tick Time
    | Today Time
    | GenerateQuestion
    | Answer String


type alias Letter =
    { name : String
    , sign : Url
    , description : String
    }


type alias Letters =
    Dict String Letter


emptyLetter =
    { name = "", description = "", sign = "" }


lettersDict =
    Dict.fromList <|
        lettersList


lettersList =
    [ ( "A", { name = "A", sign = "/static/img/a.jpg", description = "" } )
    , ( "B", { name = "B", sign = "/static/img/b.jpg", description = "" } )
    , ( "C", { name = "C", sign = "/static/img/c.jpg", description = "" } )
    , ( "D", { name = "D", sign = "/static/img/d.jpg", description = "" } )
    , ( "E", { name = "E", sign = "/static/img/e.jpg", description = "" } )
    , ( "F", { name = "F", sign = "/static/img/f.jpg", description = "" } )
    , ( "G", { name = "G", sign = "/static/img/g.jpg", description = "" } )
    , ( "H", { name = "H", sign = "/static/img/h.jpg", description = "" } )
    , ( "I", { name = "I", sign = "/static/img/i.jpg", description = "" } )
    , ( "J", { name = "J", sign = "/static/img/j.jpg", description = "" } )
    , ( "K", { name = "K", sign = "/static/img/k.jpg", description = "" } )
    , ( "L", { name = "L", sign = "/static/img/l.jpg", description = "" } )
    , ( "M", { name = "M", sign = "/static/img/m.jpg", description = "" } )
    , ( "N", { name = "N", sign = "/static/img/n.jpg", description = "" } )
    , ( "O", { name = "O", sign = "/static/img/o.jpg", description = "" } )
    , ( "P", { name = "P", sign = "/static/img/p.jpg", description = "" } )
    , ( "Q", { name = "Q", sign = "/static/img/q.jpg", description = "" } )
    , ( "R", { name = "R", sign = "/static/img/r.jpg", description = "" } )
    , ( "S", { name = "S", sign = "/static/img/s.jpg", description = "" } )
    , ( "T", { name = "T", sign = "/static/img/t.jpg", description = "" } )
    , ( "U", { name = "U", sign = "/static/img/u.jpg", description = "" } )
    , ( "V", { name = "V", sign = "/static/img/v.jpg", description = "" } )
    , ( "W", { name = "W", sign = "/static/img/w.jpg", description = "" } )
    , ( "X", { name = "X", sign = "/static/img/x.jpg", description = "" } )
    , ( "Y", { name = "Y", sign = "/static/img/y.jpg", description = "" } )
    , ( "Z", { name = "Z", sign = "/static/img/z.jpg", description = "" } )
    ]


type alias Url =
    String


type Mode
    = Signs


type Styles
    = Default


stylesheet =
    styleSheet [ style Default [] ]


type alias Model =
    { currentLetter : Letter
    , userChoice : String
    , choices : List String
    , mode : Mode
    , seed : Seed
    , time : Time
    , result : Maybe Bool
    , answered : Int
    , correct : Int
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Tick time ->
            model ! []

        GenerateQuestion ->
            let
                lettersWithoutLast =
                    lettersDict
                        |> Dict.remove model.currentLetter.name
                        |> Dict.remove model.userChoice
                        |> Dict.keys

                ( ( currentLetter, l1 ), s1 ) =
                    lettersWithoutLast
                        |> Random.List.choose
                        |> Random.map (Tuple.mapFirst (Maybe.withDefault "?"))
                        |> (flip Random.step) model.seed

                ( ( choice1, l2 ), s2 ) =
                    l1
                        |> Random.List.choose
                        |> Random.map (Tuple.mapFirst (Maybe.withDefault "?"))
                        |> (flip Random.step) s1

                ( ( choice2, l3 ), s3 ) =
                    l2
                        |> Random.List.choose
                        |> Random.map (Tuple.mapFirst (Maybe.withDefault "?"))
                        |> (flip Random.step) s2

                ( choices, s4 ) =
                    [ currentLetter, choice1, choice2 ]
                        |> Random.List.shuffle
                        |> (flip Random.step) s3
            in
                { model | seed = s4, currentLetter = currentLetter |> (flip Dict.get) lettersDict |> Maybe.withDefault emptyLetter, choices = choices, result = Nothing } ! []

        Answer userChoice ->
            let
                result =
                    userChoice == model.currentLetter.name

                answered =
                    model.answered + 1

                correct =
                    model.correct
                        + (if result then
                            1
                           else
                            0
                          )
            in
                { model | userChoice = userChoice, result = Just <| result, answered = answered, correct = correct } ! []

        Today time ->
            { model | time = time, seed = Random.initialSeed <| round model.time } ! [ Task.perform identity <| Task.succeed GenerateQuestion ]


view ({ currentLetter, choices } as model) =
    viewport stylesheet <|
        el Default [] <|
            EK.column Default
                [ spacing 10 ]
                [ ( currentLetter.sign, imageForLetter currentLetter )
                , ( currentLetter.sign ++ "choice", answers choices )
                , ( currentLetter.sign ++ "result", viewResult model )
                ]


viewResult { result, answered, correct } =
    case result of
        Nothing ->
            empty

        Just result ->
            let
                comment =
                    case result of
                        True ->
                            "Bravo !!!"

                        False ->
                            "Perdu..."
            in
                el Default [ center ] <|
                    column Default
                        [ spacing 10 ]
                        [ button Default [ padding 10, onClick GenerateQuestion ] <| text ">>"
                        , el Default [] <|
                            row Default
                                [ spacing 20 ]
                                [ text comment, text <| "Score : " ++ toString correct ++ "/" ++ toString answered ]
                        ]


imageForLetter { sign, description } =
    el Default [ center ] <|
        image Default [ width (px 100), height (px 100) ] { src = sign, caption = description }


answers choices =
    el Default [ center ] <|
        row Default [ spacing 10 ] <|
            List.map viewChoice <|
                choices


viewChoice choice =
    button Default [ padding 10, onClick <| Answer choice ] <| text <| String.toLower <| choice


init : ( Model, Cmd Msg )
init =
    ( { mode = Signs
      , userChoice = ""
      , currentLetter = emptyLetter
      , choices = []
      , seed = Random.initialSeed 0
      , time = 0
      , result = Nothing
      , answered = 0
      , correct = 0
      }
    , getNow
    )


subs model =
    -- Time.every Time.second Tick
    Sub.none


getNow =
    Time.now
        |> Task.perform Today
