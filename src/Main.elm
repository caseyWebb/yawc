module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyPress)
import Css as Css
import Dict exposing (Dict)
import Html.Styled exposing (Html, a, button, div, footer, h1, header, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Process
import Set exposing (Set)
import Task as Task
import Words exposing (charIndiciesDict, getTodaysWord, isValidWord)



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
    { gameState : GameState
    , todaysWord : String
    , todaysWordCharIndiciesDict : Dict Char (Set Int) -- key = char, value = set of indices
    , currentGuess : String
    , guesses : List (List ( Char, LetterGuessResult ))
    , letterStates : Dict Char (Maybe LetterGuessResult)
    , message : Maybe String
    , messageTimeoutId : Int
    }


type GameState
    = Playing
    | Won
    | Lost


type LetterGuessResult
    = CorrectPosition
    | IncorrectPosition
    | NotInWord


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { todaysWord = ""
      , todaysWordCharIndiciesDict = Dict.empty
      , currentGuess = ""
      , guesses = []
      , letterStates =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                |> String.toList
                |> List.map (\c -> ( c, Nothing ))
                |> Dict.fromList
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
            if model.gameState == Playing then
                ( { model | currentGuess = updatedGuess |> String.left 5 |> String.toUpper }, Cmd.none )

            else
                ( model, Cmd.none )

        SubmitCurrentGuess ->
            let
                messageTimeoutId =
                    model.messageTimeoutId + 1

                clearMessageAfter1Sec =
                    Process.sleep 1000 |> Task.perform (\_ -> ClearMessage messageTimeoutId)

                updatedGuessedWords =
                    updateGuessedWords model

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
                        , letterStates = updateLetterStates model
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

        ClearMessage messageTimeoutId ->
            if messageTimeoutId == model.messageTimeoutId then
                ( { model | message = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateGuessedWords : Model -> List (List ( Char, LetterGuessResult ))
updateGuessedWords model =
    model.guesses
        ++ [ List.indexedMap
                (\i letter ->
                    ( letter, checkLetterState model i letter )
                )
                (String.toList model.currentGuess)
           ]


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
                    (String.toList model.currentGuess)
    in
    Dict.union newStates model.letterStates


checkLetterState : Model -> Int -> Char -> LetterGuessResult
checkLetterState model guessIndex letter =
    case Dict.get letter model.todaysWordCharIndiciesDict of
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
                            String.dropRight 1 model.currentGuess
                                |> UpdateCurrentGuess

                        _ ->
                            NoOp
                )
                keyDecoder
        ]


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
        [ a
            [ href "https://github.com/caseyWebb/YAWC"
            , css
                [ Css.textDecoration Css.none
                , Css.color (Css.hex "fff")
                ]
            ]
            [ h1 [] [ text "Yet Another Wordle Clone" ] ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceAround
            , Css.alignItems Css.center
            , Css.height (Css.vh 80)
            ]
        ]
        [ div
            [ css [ Css.minHeight (Css.px 60) ] ]
            [ model.message |> Maybe.map viewMessage |> Maybe.withDefault (text "") ]
        , viewGrid model
        , viewKeyboard model
        ]


viewMessage : String -> Html Msg
viewMessage message =
    div
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
        (viewGuessedWords model.guesses
            ++ (if List.length model.guesses == 6 then
                    []

                else
                    [ viewCurrentGuess model.currentGuess
                    , viewBlankRows (5 - List.length model.guesses)
                    ]
               )
        )


viewGuessedWords : List (List ( Char, LetterGuessResult )) -> List (Html Msg)
viewGuessedWords =
    List.map (viewRow << List.map viewGuessedGridSquare)


viewFlexContainer : Css.FlexDirectionOrWrap (Css.FlexDirection {}) -> List (Html Msg) -> Html Msg
viewFlexContainer direction =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection direction
            , Css.justifyContent Css.spaceBetween
            ]
        ]


viewRow : List (Html Msg) -> Html Msg
viewRow =
    viewFlexContainer Css.row


viewColumn : List (Html Msg) -> Html Msg
viewColumn =
    viewFlexContainer Css.column


viewCurrentGuess : String -> Html Msg
viewCurrentGuess word =
    let
        letters =
            String.toList word
    in
    viewRow <|
        List.map (\l -> viewGridSquare ( l, Nothing )) letters
            ++ List.repeat (5 - List.length letters) viewEmptyGridSquare


viewBlankRows : Int -> Html Msg
viewBlankRows n =
    viewColumn <| List.repeat n (viewRow (List.repeat 5 viewEmptyGridSquare))


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
            ([ Css.width (Css.px 62)
             , Css.height (Css.px 62)
             , Css.margin (Css.px 2)
             , Css.fontWeight Css.bold
             , Css.fontSize (Css.px 32)
             , Css.lineHeight (Css.px 66)
             , Css.textAlign Css.center
             , Css.boxSizing Css.borderBox
             ]
                ++ (case letterStateColor state of
                        Nothing ->
                            [ Css.border3 (Css.px 2) Css.solid (Css.hex "3a3a3c") ]

                        Just color ->
                            [ Css.border3 (Css.px 2) Css.solid Css.transparent
                            , Css.backgroundColor color
                            ]
                   )
            )
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
            [ Dict.get letter model.letterStates
                |> Maybe.andThen identity
                |> letterStateColor
                |> Maybe.withDefault (Css.rgb 129 131 132)
                |> Css.backgroundColor
            , Css.color <| Css.rgb 255 255 255
            , Css.height (Css.px 50)
            , Css.width (Css.px 35)
            , Css.margin (Css.px 3)
            , Css.fontWeight Css.bold
            , Css.borderRadius (Css.px 4)
            , Css.borderStyle Css.none
            , Css.cursor Css.pointer
            ]
        , onClick (UpdateCurrentGuess (String.append model.currentGuess (String.fromChar letter)))
        ]
        [ text <| String.fromChar letter ]


viewFooter : Model -> Html Msg
viewFooter _ =
    let
        linkStyles =
            [ Css.textDecoration Css.none
            , Css.fontWeight Css.bold
            , Css.color (Css.hex "3a3a3c")
            ]
    in
    footer [ css [ Css.color (Css.hex "272729"), Css.padding (Css.px 5) ] ]
        [ text "Made with <3 by "
        , a [ href "https://caseyWebb.xyz", css linkStyles ] [ text "Casey Webb" ]
        ]


letterStateColor : Maybe LetterGuessResult -> Maybe Css.Color
letterStateColor =
    Maybe.map
        (\state ->
            case state of
                CorrectPosition ->
                    Css.rgb 83 141 78

                IncorrectPosition ->
                    Css.rgb 181 159 59

                NotInWord ->
                    Css.rgb 58 58 60
        )
