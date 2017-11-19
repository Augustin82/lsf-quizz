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
    | Bottom
    | Right
    | Left


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
                        [ verticalSpread, height fill, width fill, paddingTop 10 ]
                        [ ( currentLetter.sign, imageForLetter currentLetter )
                        , ( currentLetter.sign ++ "choice", answers choices )
                        ]

                Result ->
                    EK.column Default
                        [ verticalSpread, height fill, width fill, paddingTop 10 ]
                        [ ( currentLetter.sign, imageForLetter currentLetter )
                        , ( currentLetter.sign ++ "result", viewResult model )
                        ]


viewResult : Model -> Elem
viewResult { result, answered, correct, currentLetter } =
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
                            "Perdu =("
            in
                el Default [ center, width fill ] <|
                    column Default
                        [ spacing 10 ]
                        [ el Default [ center ] <| text <| "--> " ++ currentLetter.name ++ " <--"
                        , el Default [ center ] <|
                            text comment
                        , el Default [ center ] <| text <| "Score : " ++ toString correct ++ "/" ++ toString answered
                        , button Default [ padding 10, onClick GenerateQuestion ] <| text "Suivant >>"
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
        , vary Bottom (index > 1)
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
    [ ( "A", { name = "A", sign = "/static/img/hd/a.png", description = "" } )
    , ( "B", { name = "B", sign = "/static/img/hd/b.png", description = "" } )
    , ( "C", { name = "C", sign = "/static/img/hd/c.png", description = "" } )
    , ( "D", { name = "D", sign = "/static/img/hd/d.png", description = "" } )
    , ( "E", { name = "E", sign = "/static/img/hd/e.png", description = "" } )
    , ( "F", { name = "F", sign = "/static/img/hd/f.png", description = "" } )
    , ( "G", { name = "G", sign = "/static/img/hd/g.png", description = "" } )
    , ( "H", { name = "H", sign = "/static/img/hd/h.png", description = "" } )
    , ( "I", { name = "I", sign = "/static/img/hd/i.png", description = "" } )
    , ( "J", { name = "J", sign = "/static/img/hd/j.png", description = "" } )
    , ( "K", { name = "K", sign = "/static/img/hd/k.png", description = "" } )
    , ( "L", { name = "L", sign = "/static/img/hd/l.png", description = "" } )
    , ( "M", { name = "M", sign = "/static/img/hd/m.png", description = "" } )
    , ( "N", { name = "N", sign = "/static/img/hd/n.png", description = "" } )
    , ( "O", { name = "O", sign = "/static/img/hd/o.png", description = "" } )
    , ( "P", { name = "P", sign = "/static/img/hd/p.png", description = "" } )
    , ( "Q", { name = "Q", sign = "/static/img/hd/q.png", description = "" } )
    , ( "R", { name = "R", sign = "/static/img/hd/r.png", description = "" } )
    , ( "S", { name = "S", sign = "/static/img/hd/s.png", description = "" } )
    , ( "T", { name = "T", sign = "/static/img/hd/t.png", description = "" } )
    , ( "U", { name = "U", sign = "/static/img/hd/u.png", description = "" } )
    , ( "V", { name = "V", sign = "/static/img/hd/v.png", description = "" } )
    , ( "W", { name = "W", sign = "/static/img/hd/w.png", description = "" } )
    , ( "X", { name = "X", sign = "/static/img/hd/x.png", description = "" } )
    , ( "Y", { name = "Y", sign = "/static/img/hd/y.png", description = "" } )
    , ( "Z", { name = "Z", sign = "/static/img/hd/z.png", description = "" } )
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
            , variation Bottom [ Border.bottom 1 ]
            , variation Right [ Border.right 1 ]
            , variation Left [ Border.left 1 ]
            ]
        ]
