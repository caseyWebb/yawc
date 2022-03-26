module Grid exposing (view)

import Colors as Colors
import Css as Css
import Css.Animations as Anim
import Css.Transitions as Transitions
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
        List.map (\l -> viewSquare 0 (Pending l)) letters
            ++ List.repeat (5 - List.length letters) (viewSquare 0 Empty)


viewBlankRows : Int -> Html msg
viewBlankRows n =
    viewColumn <| List.repeat n (viewRow (List.repeat 5 (viewSquare 0 Empty)))


viewGuessedGridSquare : Model -> Int -> Char -> Html msg
viewGuessedGridSquare model i letter =
    Dict.get letter model.pastGuessResults
        |> Maybe.map
            (\state ->
                case state of
                    LetterGuessResult.InWord knownPositions _ ->
                        if Set.member i knownPositions then
                            viewSquare i (CorrectPosition letter)

                        else
                            viewSquare i (IncorrectPosition letter)

                    LetterGuessResult.NotInWord ->
                        viewSquare i (NotInWord letter)
            )
        |> Maybe.withDefault (viewSquare i Empty)


viewSquare : Int -> GridSquare -> Html msg
viewSquare i square =
    let
        styleDelay =
            250 * i |> toFloat

        popInAnimation =
            Css.batch
                [ Css.animationDuration (Css.ms 100)
                , Css.animationName <|
                    Anim.keyframes
                        [ ( 0
                          , [ Anim.opacity (Css.int 0)
                            , Anim.transform [ Css.scale 0.8 ]
                            ]
                          )
                        , ( 40
                          , [ Anim.opacity (Css.int 1)
                            , Anim.transform [ Css.scale 1.1 ]
                            ]
                          )
                        ]
                ]

        flipInAnimation =
            Css.batch
                [ Css.animationDuration (Css.ms 500)
                , Css.animationDelay (Css.ms styleDelay)
                , Css.property "animation-timing-function" "ease-in"
                , Css.animationName <|
                    Anim.keyframes
                        [ ( 0
                          , [ Anim.transform [ Css.rotateX (Css.deg 0) ]
                            ]
                          )
                        , ( 50
                          , [ Anim.transform [ Css.rotateX (Css.deg -90) ] ]
                          )
                        , ( 100
                          , [ Anim.transform [ Css.rotateX (Css.deg 0) ] ]
                          )
                        ]
                , Transitions.transition
                    [ Transitions.backgroundColor2 0 (styleDelay + 250)
                    , Transitions.borderColor2 0 (styleDelay + 250)
                    ]
                ]

        { maybeBgColor, borderColor, char, maybeAnimation } =
            case square of
                CorrectPosition a ->
                    { maybeBgColor = Just Colors.green
                    , borderColor = Colors.green
                    , char = a
                    , maybeAnimation = Just flipInAnimation
                    }

                IncorrectPosition a ->
                    { maybeBgColor = Just Colors.yellow
                    , borderColor = Colors.yellow
                    , char = a
                    , maybeAnimation = Just flipInAnimation
                    }

                NotInWord a ->
                    { maybeBgColor = Just Colors.gray4
                    , borderColor = Colors.gray4
                    , char = a
                    , maybeAnimation = Just flipInAnimation
                    }

                Empty ->
                    { maybeBgColor = Nothing
                    , borderColor = Colors.gray4
                    , char = ' '
                    , maybeAnimation = Nothing
                    }

                Pending a ->
                    { maybeBgColor = Nothing
                    , borderColor = Colors.gray3
                    , char = a
                    , maybeAnimation = Just popInAnimation
                    }
    in
    div
        [ css
            ([ Css.width (Css.px 62)
             , Css.height (Css.px 62)
             , Css.margin (Css.px 2.5)
             , Css.fontWeight Css.bold
             , Css.fontSize (Css.px 32)
             , Css.lineHeight (Css.px 66)
             , Css.textAlign Css.center
             , Css.boxSizing Css.borderBox
             , Css.border3 (Css.px 2) Css.solid borderColor
             ]
                ++ List.filterMap identity [ Maybe.map Css.backgroundColor maybeBgColor, maybeAnimation ]
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
