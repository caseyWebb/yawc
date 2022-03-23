module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (Key)
import Css as Css
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Json.Decode as Decode
import Set exposing (Set)



-- MAIN


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



-- MODEL


type alias Flags =
    {}


type alias Model =
    { winningWord : Dict Char (Set Int) -- key = char, value = set of indices
    , currentGuess : List Char
    , guessedWords : List (List ( Char, LetterGuessResult ))
    , letterStates : Dict Char (Maybe LetterGuessResult)
    }


type LetterGuessResult
    = CorrectPosition
    | IncorrectPosition
    | NotInWord


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { winningWord = parseWinningWord "DOLOR"
      , currentGuess = []
      , guessedWords = []
      , letterStates =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.toList
                |> List.map (\c -> ( c, Nothing ))
                |> Dict.fromList
      }
    , Cmd.none
    )


parseWinningWord : String -> Dict Char (Set Int)
parseWinningWord word =
    List.indexedMap (\i l -> ( l, i )) (String.toList word)
        |> List.foldr
            (\( l, i ) dict ->
                Dict.update l
                    (Just << Set.insert i << Maybe.withDefault (Set.singleton i))
                    dict
            )
            Dict.empty



-- UPDATE


type Msg
    = UpdateCurrentGuess (List Char)
    | SubmitCurrentGuess
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCurrentGuess updatedGuess ->
            ( { model | currentGuess = updatedGuess }, Cmd.none )

        SubmitCurrentGuess ->
            ( { model
                | currentGuess = []
                , guessedWords = updateGuessedWords model
                , letterStates = updateLetterStates model
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateGuessedWords : Model -> List (List ( Char, LetterGuessResult ))
updateGuessedWords model =
    model.guessedWords ++ [ List.indexedMap (\i letter -> ( letter, checkLetterState model i letter )) model.currentGuess ]


updateLetterStates : Model -> Dict Char (Maybe LetterGuessResult)
updateLetterStates model =
    let
        newStates =
            Dict.fromList <|
                List.indexedMap
                    (\guessIndex letter ->
                        ( letter
                        , Just <|
                            case ( checkLetterState model guessIndex letter, Dict.get letter model.letterStates |> Maybe.andThen identity ) of
                                ( CorrectPosition, _ ) ->
                                    CorrectPosition

                                ( _, Just CorrectPosition ) ->
                                    CorrectPosition

                                ( IncorrectPosition, _ ) ->
                                    IncorrectPosition

                                ( _, Just IncorrectPosition ) ->
                                    IncorrectPosition

                                ( _, _ ) ->
                                    NotInWord
                        )
                    )
                    model.currentGuess
    in
    Dict.union newStates model.letterStates


checkLetterState : Model -> Int -> Char -> LetterGuessResult
checkLetterState model guessIndex letter =
    case Dict.get letter model.winningWord of
        Nothing ->
            NotInWord

        Just actualIndicies ->
            if Set.member guessIndex actualIndicies then
                CorrectPosition

            else
                IncorrectPosition



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown <|
        Decode.map
            (\key ->
                case key of
                    Enter ->
                        if List.length model.currentGuess == 5 then
                            SubmitCurrentGuess

                        else
                            NoOp

                    Backspace ->
                        UpdateCurrentGuess <| Maybe.withDefault [] <| Maybe.map List.reverse <| List.tail <| List.reverse model.currentGuess

                    Character c ->
                        UpdateCurrentGuess <| List.take 5 (model.currentGuess ++ [ Char.toUpper c ])

                    _ ->
                        NoOp
            )
            keyDecoder


type Key
    = Character Char
    | Enter
    | Backspace
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

    else
        case String.uncons string of
            Just ( char, "" ) ->
                Character char

            _ ->
                Other string



-- VIEW


view : Model -> Html Msg
view model =
    div
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
        [ h1 [] [ text "Yet Another Wordle Clone" ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    div
        [ css
            []
        ]
        [ viewGrid model
        , viewKeyboard model
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            ]
        ]
        [ viewGuessedWords model.guessedWords
        , viewCurrentGuess model.currentGuess
        ]


viewGuessedWords : List (List ( Char, LetterGuessResult )) -> Html Msg
viewGuessedWords =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        << List.map (viewRow << List.map viewGuessedGridSquare)


viewRow : List (Html Msg) -> Html Msg
viewRow =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            ]
        ]


viewCurrentGuess : List Char -> Html Msg
viewCurrentGuess letters =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            ]
        ]
    <|
        List.map (\l -> viewGridSquare ( l, Nothing )) letters
            ++ List.repeat (5 - List.length letters) viewEmptyGridSquare


viewEmptyGridSquare : Html Msg
viewEmptyGridSquare =
    viewGridSquare ( ' ', Nothing )


viewGuessedGridSquare : ( Char, LetterGuessResult ) -> Html Msg
viewGuessedGridSquare ( letter, state ) =
    viewGridSquare ( letter, Just state )


viewGridSquare : ( Char, Maybe LetterGuessResult ) -> Html Msg
viewGridSquare ( letter, state ) =
    div
        [ css
            [ Css.width (Css.px 50)
            , Css.height (Css.px 50)
            , Css.borderRadius (Css.px 5)
            , Css.border3 (Css.px 1) Css.solid (Css.rgb 255 255 255)
            , Css.backgroundColor (letterStateColor state)
            ]
        ]
        [ text <| String.fromChar letter ]


viewKeyboard : Model -> Html Msg
viewKeyboard model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        ([ "QWERTYUIOP", "ASDFGHJKL", "ZXCVBNM" ] |> List.map (viewKeyboardRow model))


viewKeyboardRow : Model -> String -> Html Msg
viewKeyboardRow model row =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.center
            ]
        ]
        (List.map (viewKeyboardButton model) (String.toList row))


viewKeyboardButton : Model -> Char -> Html Msg
viewKeyboardButton model letter =
    button
        [ css
            [ Css.backgroundColor <| letterStateColor <| Maybe.andThen identity <| Dict.get letter model.letterStates
            , Css.color <| Css.rgb 255 255 255
            ]
        ]
        [ text <| String.fromChar letter ]


viewFooter : Model -> Html Msg
viewFooter _ =
    footer []
        [ text "Made with <3 by "
        , a [ href "https://caseyWebb.xyz" ] [ text "Casey Webb" ]
        ]


letterStateColor : Maybe LetterGuessResult -> Css.Color
letterStateColor state =
    case state of
        Just CorrectPosition ->
            Css.rgb 83 141 78

        Just IncorrectPosition ->
            Css.rgb 181 159 59

        Just NotInWord ->
            Css.rgb 58 58 60

        Nothing ->
            Css.rgba 0 0 0 0
