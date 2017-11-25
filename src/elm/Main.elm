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
    | GoHome


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
    | Jumbo


type Variations
    = None
    | Smaller
    | Smallest
    | Larger
    | Top
    | Bottom
    | Right
    | Left
    | White
    | Primary
    | Secondary


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
    case Debug.log "msg" msg of
        NoOp ->
            model ! []

        GoHome ->
            { model | state = Home } ! []

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
                                    1
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
                    model.answered + 1

                correct =
                    model.correct
                        + (if match then
                            1
                           else
                            0
                          )
            in
                { model
                    | userChoice = userChoice
                    , result = result
                    , answered = answered
                    , correct = correct
                    , state = Score
                }
                    ! []

        Today time ->
            { model | time = time, seed = Random.initialSeed <| round time } ! []


view : Model -> Html Msg
view ({ currentLetter, choices, state, counter, mode } as model) =
    viewport stylesheet <|
        el Main [ height fill, width fill, clipX, clipY ] <|
            case state of
                Home ->
                    el Default [ verticalCenter, center ] <|
                        column Default
                            [ spacingXY 0 20 ]
                            [ el Default [ vary Larger True ] <|
                                text "Bienvenue !"
                            , spacer 4
                            , button Button [ vary Smaller True, vary Primary True, padding 10, onClick <| GenerateQuestion RecognizeSign ] <|
                                text "Reconnaître un signe"
                            , button Button [ vary Smaller True, vary Primary True, padding 10, onClick <| GenerateQuestion SelectSign ] <|
                                text "Choisir le bon signe"
                            , copyrightNotice
                            ]

                Question ->
                    column Default
                        [ height fill, width fill, verticalSpread ]
                        [ viewQuestion mode currentLetter
                        , viewAnswers model
                        , homeButton
                        ]

                Score ->
                    column Default
                        [ height fill, width fill, verticalSpread ]
                        [ viewQuestion mode currentLetter
                        , viewScore model
                        , homeButton
                        ]


copyrightNotice : Elem
copyrightNotice =
    screen <|
        el Default [ alignRight, alignBottom, padding 10, vary Secondary True, vary Smallest True ] <|
            text "© Augustin Ragon 2017 - Tous droits réservés"


homeButton : Elem
homeButton =
    screen <|
        button Button [ vary Primary True, onClick GoHome, padding 5 ] <|
            text "<<"


progressBar : Int -> Elem
progressBar counter =
    el Default [ width (percent 100), alignBottom ] <|
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
                            "Bravo !"

                        Incorrect ->
                            "Perdu !"

                        Timeout ->
                            "Trop tard !"

                        NotAnswered ->
                            ""
            in
                el Default [ center, height fill, width fill ] <|
                    column Default
                        [ height fill, verticalSpread, width fill ]
                        [ el Default [ height <| px <| defSize + 10 ] <|
                            viewAnswer mode currentLetter
                        , el Default [ vary Secondary True, center ] <|
                            text comment
                        , el Default [ vary Secondary True, center, paddingXY 10 20 ] <|
                            text ("Score : " ++ toString correct ++ "/" ++ toString answered)
                        , button Button
                            [ padding 10
                            , vary Primary True
                            , onClick <|
                                GenerateQuestion mode
                            ]
                          <|
                            text "Suivant >>"
                        ]


viewQuestion : Mode -> Letter -> Elem
viewQuestion mode =
    case mode of
        RecognizeSign ->
            imageForLetter

        SelectSign ->
            characterForLetter


viewAnswer : Mode -> Letter -> Elem
viewAnswer mode =
    case mode of
        RecognizeSign ->
            characterForLetter

        SelectSign ->
            imageForLetter


characterForLetter : Letter -> Elem
characterForLetter { name, description } =
    el Jumbo [ center, verticalCenter ] <|
        text name


imageForLetter : Letter -> Elem
imageForLetter { sign, description } =
    el Default [ vary White True, height fill, width fill ] <|
        el Default [ center, verticalCenter ] <|
            image Default [ height (px defSize) ] { src = sign, caption = description }


viewAnswers : Model -> Elem
viewAnswers { mode, choices, counter } =
    el Default [ center, width fill ] <|
        column Default
            [ height fill, width fill ]
            [ progressBar counter
            , answers mode choices
            ]


answers : Mode -> List String -> Elem
answers mode choices =
    el Default [ center, width fill ] <|
        EK.wrappedRow Default [ alignBottom, width fill ] <|
            List.indexedMap (viewChoice mode) <|
                choices


viewChoice : Mode -> Int -> String -> ( String, Elem )
viewChoice mode index choice =
    case mode of
        RecognizeSign ->
            choice
                |> text
                |> el Jumbo [ center, verticalCenter ]
                |> button Button
                    [ height <| px <| defSize + 10
                    , width (percent 50)
                    , onClick (Answer choice)
                    , vary Secondary True
                    , vary Larger True
                    , vary Top True
                    , vary Right (rem index 2 == 0)
                    , vary Bottom (index > 1)
                    ]
                |> (,) choice

        SelectSign ->
            choice
                |> (flip Dict.get) lettersDict
                |> Maybe.withDefault emptyLetter
                |> (\{ sign, description } ->
                        image Default
                            [ center
                            , verticalCenter
                            , height (px defSize)
                            ]
                            { src = sign, caption = description }
                   )
                |> button Button
                    [ height <| px <| defSize + 10
                    , width (percent 50)
                    , onClick (Answer choice)
                    , vary White True
                    , vary Larger True
                    , vary Top True
                    , vary Right (rem index 2 == 0)
                    , vary Bottom (index > 1)
                    ]
                |> (,) choice


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


darkestIndigo : Color.Color
darkestIndigo =
    Color.rgba 9 49 69 1


lilia : Color.Color
lilia =
    Color.rgba 241 243 244 1


lighterDaisy : Color.Color
lighterDaisy =
    Color.rgba 239 212 105 1


raven : Color.Color
raven =
    Color.rgba 58 62 64 1


defSize : Float
defSize =
    150


stylesheet : StyleSheet Styles Variations
stylesheet =
    styleSheet
        [ style Default
            [ variation Larger [ Font.size (scaled 4) ]
            , variation Smaller [ Font.size (scaled 2) ]
            , variation Smallest [ Font.size (scaled 1) ]
            , variation White
                [ SC.background lilia
                , SC.text raven
                ]
            , variation Secondary [ SC.text lighterDaisy ]
            ]
        , style Main
            [ Font.size (scaled 3)
            , Font.typeface [ Font.font "Roboto" ]
            , Font.weight 400
            , SC.text lilia
            , SC.background darkestIndigo
            ]
        , style Jumbo
            [ Font.size <| defSize - 25 ]
        , style Button
            [ Border.solid
            , variation Larger [ Font.size (scaled 4) ]
            , variation Smaller [ Font.size (scaled 2) ]
            , variation White
                [ SC.background lilia
                , SC.text raven
                ]
            , variation Primary
                [ SC.background lighterDaisy
                , SC.text raven
                ]
            , variation Secondary
                [ SC.background darkestIndigo
                , SC.text lilia
                ]
            , variation Top [ Border.top 1 ]
            , variation Bottom [ Border.bottom 1 ]
            , variation Right [ Border.right 1 ]
            , variation Left [ Border.left 1 ]
            ]
        , style Bar
            [ SC.background lighterDaisy
            , Transition.transitions [ { delay = 0, duration = 1000, easing = "linear", props = [ "width" ] } ]
            ]
        ]
