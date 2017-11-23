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
import Style.Transition as Transition
import Style.Color as SC
import Color
import Dict exposing (Dict)
import Random exposing (Seed)
import Time exposing (Time)
import Task
import Random
import Random.List
import Assets exposing (..)


type Msg
    = NoOp
    | Tick Time
    | Today Time
    | GenerateQuestion Mode
    | Answer String


type alias Model =
    { currentLetter : Letter
    , userChoice : String
    , choices : List String
    , mode : Mode
    , seed : Seed
    , time : Time
    , result : Result
    , answered : Int
    , correct : Int
    , state : State
    , counter : Int
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
    = RecognizeSign
    | SelectSign


type State
    = Home
    | Question
    | Score


type Result
    = Correct
    | Incorrect
    | Timeout
    | NotAnswered


type Styles
    = Default
    | Main
    | Button
    | Bar


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
            case model.state of
                Question ->
                    let
                        newCount =
                            model.counter - 1

                        timeout =
                            model.counter < 0

                        answered =
                            model.answered
                                + (if timeout then
                                    10
                                   else
                                    0
                                  )

                        result =
                            if timeout then
                                Timeout
                            else
                                model.result

                        state =
                            if timeout then
                                Score
                            else
                                model.state
                    in
                        { model
                            | state = state
                            , counter = newCount
                            , answered = answered
                            , result = result
                        }
                            ! []

                _ ->
                    model ! []

        GenerateQuestion mode ->
            let
                lettersWithoutLast =
                    lettersDict
                        |> Dict.remove model.currentLetter.name
                        |> Dict.remove model.userChoice
                        |> Dict.keys

                ( choices, newSeed1 ) =
                    lettersWithoutLast
                        |> Random.List.shuffle
                        |> Random.map (List.take 4)
                        |> (flip Random.step) model.seed

                ( ( randomLetter, _ ), newSeed2 ) =
                    choices
                        |> Random.List.choose
                        |> (flip Random.step) newSeed1
            in
                { model
                    | seed = newSeed2
                    , currentLetter =
                        randomLetter
                            |> Maybe.withDefault ""
                            |> (flip Dict.get) lettersDict
                            |> Maybe.withDefault emptyLetter
                    , choices = choices
                    , result = NotAnswered
                    , state = Question
                    , counter = 10
                    , mode = mode
                }
                    ! []

        Answer userChoice ->
            let
                match =
                    userChoice == model.currentLetter.name

                result =
                    if match then
                        Correct
                    else
                        Incorrect

                answered =
                    model.answered + 10

                correct =
                    model.correct
                        + (if match then
                            model.counter
                           else
                            0
                          )
            in
                { model | userChoice = userChoice, result = result, answered = answered, correct = correct, state = Score } ! []

        Today time ->
            { model | time = time, seed = Random.initialSeed <| round time } ! []


view : Model -> Html Msg
view ({ currentLetter, choices, state, counter, mode } as model) =
    viewport stylesheet <|
        el Main [ height fill, width fill ] <|
            case state of
                Home ->
                    el Default [ verticalCenter, center ] <|
                        column Default
                            [ spacing 20 ]
                            [ text "Bienvenue !"
                            , spacer 4
                            , button Button [ onClick <| GenerateQuestion RecognizeSign ] <| text "Reconnaître un signe"
                            , button Button [ onClick <| GenerateQuestion SelectSign ] <| text "Choisir le bon signe"
                            ]

                Question ->
                    EK.column Default
                        [ verticalSpread, height fill, width fill, paddingTop 10 ]
                        [ ( currentLetter.name, viewQuestion mode currentLetter )
                        , ( currentLetter.name ++ "progressBar", progressBar counter )
                        , ( currentLetter.name ++ "choice", answers mode choices )
                        ]

                Score ->
                    EK.column Default
                        [ verticalSpread, height fill, width fill, paddingTop 10 ]
                        [ ( currentLetter.name, viewQuestion mode currentLetter )
                        , ( currentLetter.name ++ "result", viewScore model )
                        ]


progressBar : Int -> Elem
progressBar counter =
    el Default [ width (percent 100) ] <|
        el Bar [ width <| percent <| toFloat <| counter * 10 ] <|
            text " "


viewScore : Model -> Elem
viewScore { result, answered, correct, currentLetter, mode } =
    case result of
        NotAnswered ->
            empty

        _ ->
            let
                comment =
                    case result of
                        Correct ->
                            "Vrai !"

                        Incorrect ->
                            "Faux !"

                        Timeout ->
                            "Plus de temps !"

                        NotAnswered ->
                            ""
            in
                el Default [ center, width fill ] <|
                    column Default
                        [ spacing 10 ]
                        [ el Default [ center ] <| text <| "--> " ++ currentLetter.name ++ " <--"
                        , el Default [ center ] <|
                            text comment
                        , el Default [ center ] <| text <| "Score : " ++ toString correct ++ "/" ++ toString answered
                        , button Default [ padding 10, onClick <| GenerateQuestion mode ] <| text "Suivant >>"
                        ]


viewQuestion mode =
    case mode of
        RecognizeSign ->
            imageForLetter

        SelectSign ->
            characterForLetter


characterForLetter : Letter -> Elem
characterForLetter { name, description } =
    el Default [ center, width fill, height fill ] <|
        text name


imageForLetter : Letter -> Elem
imageForLetter { sign, description } =
    el Default [ center, width fill, height fill ] <|
        image Default [ width (percent 100) ] { src = sign, caption = description }


answers : Mode -> List String -> Elem
answers mode choices =
    el Default [ center, width fill ] <|
        wrappedRow Default [] <|
            List.indexedMap (viewChoice mode) <|
                choices


viewChoice : Mode -> Int -> String -> Elem
viewChoice mode index choice =
    case mode of
        RecognizeSign ->
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

        SelectSign ->
            choice
                |> (flip Dict.get) lettersDict
                |> Maybe.withDefault emptyLetter
                |> (\{ sign, description } ->
                        image Default
                            [ width (percent 100)
                            , onClick (Answer choice)
                            ]
                            { src = sign, caption = description }
                   )


init : ( Model, Cmd Msg )
init =
    ( { mode = RecognizeSign
      , state = Home
      , userChoice = ""
      , currentLetter = emptyLetter
      , choices = []
      , seed = Random.initialSeed 0
      , time = 0
      , result = NotAnswered
      , answered = 0
      , correct = 0
      , counter = 0
      }
    , getNow
    )


subs : Model -> Sub Msg
subs model =
    if model.state == Question then
        Time.every Time.second Tick
    else
        Sub.none



-- Sub.none


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
    [ ( "A", { name = "A", sign = path letterA, description = "" } )
    , ( "B", { name = "B", sign = path letterB, description = "" } )
    , ( "C", { name = "C", sign = path letterC, description = "" } )
    , ( "D", { name = "D", sign = path letterD, description = "" } )
    , ( "E", { name = "E", sign = path letterE, description = "" } )
    , ( "F", { name = "F", sign = path letterF, description = "" } )
    , ( "G", { name = "G", sign = path letterG, description = "" } )
    , ( "H", { name = "H", sign = path letterH, description = "" } )
    , ( "I", { name = "I", sign = path letterI, description = "" } )
    , ( "J", { name = "J", sign = path letterJ, description = "" } )
    , ( "K", { name = "K", sign = path letterK, description = "" } )
    , ( "L", { name = "L", sign = path letterL, description = "" } )
    , ( "M", { name = "M", sign = path letterM, description = "" } )
    , ( "N", { name = "N", sign = path letterN, description = "" } )
    , ( "O", { name = "O", sign = path letterO, description = "" } )
    , ( "P", { name = "P", sign = path letterP, description = "" } )
    , ( "Q", { name = "Q", sign = path letterQ, description = "" } )
    , ( "R", { name = "R", sign = path letterR, description = "" } )
    , ( "S", { name = "S", sign = path letterS, description = "" } )
    , ( "T", { name = "T", sign = path letterT, description = "" } )
    , ( "U", { name = "U", sign = path letterU, description = "" } )
    , ( "V", { name = "V", sign = path letterV, description = "" } )
    , ( "W", { name = "W", sign = path letterW, description = "" } )
    , ( "X", { name = "X", sign = path letterX, description = "" } )
    , ( "Y", { name = "Y", sign = path letterY, description = "" } )
    , ( "Z", { name = "Z", sign = path letterZ, description = "" } )
    ]


path : Asset -> String
path (Asset str) =
    str


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
        , style Bar
            [ SC.background Color.blue
            , Transition.transitions [ { delay = 0, duration = 1000, easing = "linear", props = [ "width" ] } ]
            ]
        ]
