module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
import Css as Css
import Dict exposing (Dict)
import Grid as Grid
import Html.Styled exposing (Html, a, div, footer, h1, header, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Keyboard as Keyboard
import LetterGuessResult exposing (..)
import Process
import Set exposing (Set)
import Solver
import Task as Task
import Words exposing (charIndiciesDict, getTodaysWord, isValidWord)


sourceCodeUrl =
    "https://github.com/caseyWebb/YAWC"



-- MAIN


type alias Flags =
    {}


type alias Model =
    { gameState : GameState
    , todaysWord : String
    , todaysWordCharIndiciesDict : Dict Char (Set Int) -- key = char, value = set of indices
    , currentGuess : String
    , guesses : List String
    , guessResults : Dict Char LetterGuessResult
    , message : Maybe String
    , messageTimeoutId : Int
    }


type GameState
    = Playing
    | Won
    | Lost


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Yet Another Wordle Clone"
                , body = [ view model |> toUnstyled ]
                }
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { todaysWord = ""
      , todaysWordCharIndiciesDict = Dict.empty
      , currentGuess = ""
      , guesses = []
      , guessResults = Dict.empty
      , message = Nothing
      , messageTimeoutId = 0
      , gameState = Playing
      }
    , Task.perform GotTodaysWord getTodaysWord
    )



-- UPDATE


type Msg
    = GotTodaysWord String
    | UpdateCurrentGuess String
    | SubmitCurrentGuess
    | ClearMessage Int
    | GetHint
    | GotHint String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodaysWord todaysWord ->
            ( { model
                | todaysWord = todaysWord
                , todaysWordCharIndiciesDict = charIndiciesDict todaysWord
              }
            , Cmd.none
            )

        UpdateCurrentGuess updatedGuess ->
            updateCurrentGuess model updatedGuess

        SubmitCurrentGuess ->
            submitCurrentGuess model

        ClearMessage messageTimeoutId ->
            if messageTimeoutId == model.messageTimeoutId then
                ( { model | message = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        GetHint ->
            ( model, Solver.generateNextGuess GotHint model.todaysWord model.guessResults )

        GotHint hint ->
            updateCurrentGuess model hint
                |> Tuple.first
                |> submitCurrentGuess

        NoOp ->
            ( model, Cmd.none )


updateCurrentGuess : Model -> String -> ( Model, Cmd msg )
updateCurrentGuess model updatedGuess =
    if model.gameState == Playing then
        ( { model | currentGuess = updatedGuess |> String.left 5 |> String.toUpper }, Cmd.none )

    else
        ( model, Cmd.none )


submitCurrentGuess : Model -> ( Model, Cmd Msg )
submitCurrentGuess model =
    let
        messageTimeoutId =
            model.messageTimeoutId + 1

        clearMessageAfter1Sec =
            Process.sleep 1000 |> Task.perform (\_ -> ClearMessage messageTimeoutId)

        updatedGuessedWords =
            model.guesses ++ [ model.currentGuess ]

        updatedGameState =
            if List.length updatedGuessedWords == 6 then
                Lost

            else if model.currentGuess == model.todaysWord then
                Won

            else
                Playing
    in
    if model.gameState == Playing then
        if String.length model.currentGuess < 5 then
            ( { model
                | message = Just "Not enough letters"
                , messageTimeoutId = messageTimeoutId
              }
            , clearMessageAfter1Sec
            )

        else if isValidWord model.currentGuess then
            ( { model
                | currentGuess = ""
                , guesses = updatedGuessedWords
                , guessResults = updateLetterStates model
                , gameState = updatedGameState
                , message =
                    case updatedGameState of
                        Playing ->
                            Nothing

                        Won ->
                            Just "You win!"

                        Lost ->
                            Just model.todaysWord
                , messageTimeoutId = messageTimeoutId
              }
            , Cmd.none
            )

        else
            ( { model
                | message = Just ("\"" ++ model.currentGuess ++ "\" not in word list")
                , messageTimeoutId = messageTimeoutId
              }
            , clearMessageAfter1Sec
            )

    else
        ( model, Cmd.none )


updateLetterStates : Model -> Dict Char LetterGuessResult
updateLetterStates model =
    List.foldr
        (\( guessIndex, letter ) acc ->
            Dict.union
                (case
                    ( checkLetterState model guessIndex letter
                    , Dict.get letter acc
                    )
                 of
                    ( InWord newKnownGood newKnownBad, Just (InWord existingKnownGood existingKnownBad) ) ->
                        Dict.singleton letter <| InWord (Set.union newKnownGood existingKnownGood) (Set.union newKnownBad existingKnownBad)

                    ( InWord knownGood knownBad, _ ) ->
                        Dict.singleton letter <| InWord knownGood knownBad

                    ( _, _ ) ->
                        Dict.singleton letter NotInWord
                )
                acc
        )
        model.guessResults
        (String.toList model.currentGuess |> List.indexedMap Tuple.pair)


checkLetterState : Model -> Int -> Char -> LetterGuessResult
checkLetterState model guessIndex letter =
    case Dict.get letter model.todaysWordCharIndiciesDict of
        Nothing ->
            NotInWord

        Just actualIndicies ->
            if Set.member guessIndex actualIndicies then
                InWord (Set.singleton guessIndex) Set.empty

            else
                InWord Set.empty (Set.singleton guessIndex)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyPress <|
            Decode.map
                (\key ->
                    case key of
                        Character c ->
                            if Char.isAlpha c then
                                String.fromChar c
                                    |> String.append model.currentGuess
                                    |> UpdateCurrentGuess

                            else
                                NoOp

                        _ ->
                            NoOp
                )
                keyDecoder
        , onKeyDown <|
            Decode.map
                (\key ->
                    case key of
                        Enter ->
                            SubmitCurrentGuess

                        Backspace ->
                            UpdateCurrentGuess <| backspace model.currentGuess

                        Question ->
                            GetHint

                        _ ->
                            NoOp
                )
                keyDecoder
        ]


type Key
    = Character Char
    | Enter
    | Backspace
    | Question
    | Other String


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    if string == "Enter" then
        Enter

    else if string == "Backspace" then
        Backspace

    else if string == "?" then
        Question

    else
        case String.uncons string of
            Just ( char, "" ) ->
                Character char

            _ ->
                Other string



-- VIEW


view : Model -> Html Msg
view model =
    Html.Styled.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.height (Css.vh 100)
            , Css.backgroundColor (Css.rgb 18 18 19)
            , Css.color (Css.rgb 255 255 255)
            , Css.fontFamilies [ "Helvetica Neue", "Helvetica", "Roboto", "Open Sans", "Arial", "sans-serif" ]
            ]
        ]
        [ viewHeader model
        , viewBody model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader _ =
    header []
        [ a
            [ href sourceCodeUrl
            , css
                [ Css.textDecoration Css.none
                , Css.color (Css.hex "fff")
                ]
            ]
            [ h1 [] [ text "Yet Another Wordle Clone" ] ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    Html.Styled.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceAround
            , Css.alignItems Css.center
            , Css.height (Css.vh 80)
            ]
        ]
        [ Html.Styled.div
            [ css [ Css.minHeight (Css.px 60) ] ]
            [ model.message |> Maybe.map viewMessage |> Maybe.withDefault (text "") ]
        , Grid.view
            { currentGuess = model.currentGuess
            , pastGuesses = model.guesses
            , pastGuessResults = model.guessResults
            }
        , Keyboard.view
            { pastGuessResults = model.guessResults
            }
            (\msg ->
                case msg of
                    Keyboard.Letter letter ->
                        UpdateCurrentGuess <| String.append model.currentGuess (String.fromChar letter)

                    Keyboard.Backspace ->
                        UpdateCurrentGuess <| backspace model.currentGuess

                    Keyboard.Enter ->
                        SubmitCurrentGuess
            )
        ]


viewMessage : String -> Html Msg
viewMessage message =
    Html.Styled.div
        [ css
            [ Css.backgroundColor (Css.hex "fff")
            , Css.color (Css.hex "000")
            , Css.fontSize (Css.px 18)
            , Css.fontWeight Css.bold
            , Css.padding (Css.px 20)
            , Css.borderRadius (Css.px 4)
            ]
        ]
        [ text message ]


viewFooter : Model -> Html Msg
viewFooter _ =
    let
        linkStyles =
            [ Css.textDecoration Css.none
            , Css.fontWeight Css.bold
            , Css.color (Css.hex "3a3a3c")
            ]
    in
    footer
        [ css
            [ Css.color (Css.hex "272729")
            , Css.padding (Css.rem 1)
            , Css.fontSize (Css.rem 1.2)
            ]
        ]
        [ Html.Styled.div
            [ css
                [ Css.textAlign Css.center
                ]
            ]
            [ text "Made with <3 by "
            , a [ href "https://caseyWebb.xyz", css linkStyles ] [ text "Casey Webb" ]
            , a
                [ href sourceCodeUrl
                , css
                    (linkStyles
                        ++ [ Css.paddingTop (Css.rem 0.5)
                           , Css.display Css.block
                           ]
                    )
                ]
                [ text "Source Code" ]
            ]
        , Html.Styled.div
            [ css
                [ Css.position Css.fixed
                , Css.bottom (Css.rem 1)
                , Css.right (Css.rem 1)
                , Css.cursor Css.pointer
                ]
            , onClick GetHint
            ]
            [ text "Press ? or click here for a hint" ]
        ]



-- UTILS


backspace : String -> String
backspace =
    String.dropRight 1
