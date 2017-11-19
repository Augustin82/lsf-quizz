module Main exposing (..)

import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Keyed as EK
import Style exposing (..)
import Style.Font as Font
import Style.Scale as Scale
import Style.Border as Border
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
    , state : State
    }


type alias Letter =
    { name : String
    , sign : Url
    , description : String
    }


type alias Letters =
    Dict String Letter


type alias Url =
    String


type Mode
    = Signs


type State
    = Home
    | Question
    | Result


type Styles
    = Default
    | Main
    | Button


type Variations
    = None
    | Larger
    | Top
    | Right


type alias Elem =
    Element Styles Variations Msg


main : Program Never Model Msg
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

                ( ( choice3, l4 ), s4 ) =
                    l3
                        |> Random.List.choose
                        |> Random.map (Tuple.mapFirst (Maybe.withDefault "?"))
                        |> (flip Random.step) s3

                ( choices, s5 ) =
                    [ currentLetter, choice1, choice2, choice3 ]
                        |> Random.List.shuffle
                        |> (flip Random.step) s4
            in
                { model | seed = s5, currentLetter = currentLetter |> (flip Dict.get) lettersDict |> Maybe.withDefault emptyLetter, choices = choices, result = Nothing, state = Question } ! []

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
                { model | userChoice = userChoice, result = Just <| result, answered = answered, correct = correct, state = Result } ! []

        Today time ->
            { model | time = time, seed = Random.initialSeed <| round model.time } ! [ Task.perform identity <| Task.succeed GenerateQuestion ]


view : Model -> Html Msg
view ({ currentLetter, choices, state } as model) =
    viewport stylesheet <|
        el Main [ height fill, width fill ] <|
            case state of
                Home ->
                    el Default [ verticalCenter, center ] <|
                        column Default
                            [ spacing 20 ]
                            [ text "Bienvenue !"
                            , spacer 4
                            , button Button [ onClick GenerateQuestion ] <| text "Jouer !"
                            ]

                Question ->
                    EK.column Default
                        [ verticalSpread, height fill, width fill ]
                        [ ( currentLetter.sign, imageForLetter currentLetter )
                        , ( currentLetter.sign ++ "choice", answers choices )
                        ]

                Result ->
                    EK.column Default
                        [ verticalSpread, height fill, width fill ]
                        [ ( currentLetter.sign, imageForLetter currentLetter )
                        , ( currentLetter.sign ++ "result", viewResult model )
                        ]


viewResult : Model -> Elem
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
                        [ button Default [ padding 10, onClick GenerateQuestion ] <| text "Suivant >>"
                        , el Default [] <|
                            text comment
                        , el Default [] <| text <| "Score : " ++ toString correct ++ "/" ++ toString answered
                        ]


imageForLetter : Letter -> Elem
imageForLetter { sign, description } =
    el Default [ center, width fill, height fill ] <|
        image Default [ width (percent 100) ] { src = sign, caption = description }


answers : List String -> Elem
answers choices =
    el Default [ center, width fill ] <|
        wrappedRow Default [] <|
            List.indexedMap viewChoice <|
                choices


viewChoice : Int -> String -> Elem
viewChoice index choice =
    button Button
        [ height (px 100)
        , width (percent 50)
        , onClick (Answer choice)
        , vary Larger True
        , vary Top True
        , vary Right (rem index 2 == 0)
        ]
    <|
        text (choice)


init : ( Model, Cmd Msg )
init =
    ( { mode = Signs
      , state = Home
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


subs : Model -> Sub Msg
subs model =
    -- Time.every Time.second Tick
    Sub.none


getNow : Cmd Msg
getNow =
    Time.now
        |> Task.perform Today


emptyLetter : Letter
emptyLetter =
    { name = "", description = "", sign = "" }


lettersDict : Dict String Letter
lettersDict =
    Dict.fromList <|
        lettersList


lettersList : List ( String, Letter )
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


scaled : Int -> Float
scaled =
    Scale.modular 12 1.618


stylesheet : StyleSheet Styles Variations
stylesheet =
    styleSheet
        [ style Default []
        , style Main
            [ Font.size (scaled 3)
            , Font.typeface [ Font.font "Roboto" ]
            , Font.weight 400
            ]
        , style Button
            [ variation Larger [ Font.size (scaled 4) ]
            , Border.solid
            , variation Top [ Border.top 1 ]
            , variation Right [ Border.right 1 ]
            ]
        ]
