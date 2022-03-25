module Grid exposing (view)

import Colors as Colors
import Css as Css
import Dict as Dict exposing (Dict)
import Html.Styled exposing (Html, a, div, text)
import Html.Styled.Attributes exposing (css)
import LetterGuessResult exposing (LetterGuessResult(..))
import Set as Set


type GridSquare
    = CorrectPosition Char
    | IncorrectPosition Char
    | NotInWord Char
    | Pending Char
    | Empty


type alias Model =
    { currentGuess : String
    , pastGuesses : List String
    , pastGuessResults : Dict Char LetterGuessResult
    }


view : Model -> Html msg
view model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            ]
        ]
        (viewGuessedWords model
            ++ (if List.length model.pastGuesses == 6 then
                    []

                else
                    [ viewCurrentGuess model.currentGuess
                    , viewBlankRows (5 - List.length model.pastGuesses)
                    ]
               )
        )


viewGuessedWords : Model -> List (Html msg)
viewGuessedWords model =
    List.map (viewRow << List.indexedMap (viewGuessedGridSquare model) << String.toList) model.pastGuesses


viewCurrentGuess : String -> Html msg
viewCurrentGuess word =
    let
        letters =
            String.toList word
    in
    viewRow <|
        List.map (\l -> viewSquare (Pending l)) letters
            ++ List.repeat (5 - List.length letters) (viewSquare Empty)


viewBlankRows : Int -> Html msg
viewBlankRows n =
    viewColumn <| List.repeat n (viewRow (List.repeat 5 (viewSquare Empty)))


viewGuessedGridSquare : Model -> Int -> Char -> Html msg
viewGuessedGridSquare model i letter =
    Dict.get letter model.pastGuessResults
        |> Maybe.map
            (\state ->
                case state of
                    LetterGuessResult.InWord knownPositions _ ->
                        if Set.member i knownPositions then
                            viewSquare (CorrectPosition letter)

                        else
                            viewSquare (IncorrectPosition letter)

                    LetterGuessResult.NotInWord ->
                        viewSquare (NotInWord letter)
            )
        |> Maybe.withDefault (viewSquare Empty)


viewSquare : GridSquare -> Html msg
viewSquare square =
    let
        ( maybeBgColor, borderColor, char ) =
            case square of
                CorrectPosition a ->
                    ( Just Colors.green, Colors.green, a )

                IncorrectPosition a ->
                    ( Just Colors.yellow, Colors.yellow, a )

                NotInWord a ->
                    ( Just Colors.gray, Colors.gray, a )

                Empty ->
                    ( Nothing, Colors.gray, ' ' )

                Pending a ->
                    ( Nothing, Colors.gray, a )
    in
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
             , Css.border3 (Css.px 2) Css.solid borderColor
             ]
                ++ List.filterMap (Maybe.map Css.backgroundColor) [ maybeBgColor ]
            )
        ]
        [ text <| String.fromChar char ]


viewFlexContainer : Css.FlexDirectionOrWrap (Css.FlexDirection {}) -> List (Html msg) -> Html msg
viewFlexContainer direction =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection direction
            , Css.justifyContent Css.spaceBetween
            ]
        ]


viewRow : List (Html msg) -> Html msg
viewRow =
    viewFlexContainer Css.row


viewColumn : List (Html msg) -> Html msg
viewColumn =
    viewFlexContainer Css.column
