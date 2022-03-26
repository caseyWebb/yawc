module Solver exposing (nextGuess)

import Array
import Dict exposing (Dict)
import LetterGuessResult exposing (LetterGuessResult(..))
import Set
import Words exposing (commonWords)


nextGuess : Dict Char LetterGuessResult -> String
nextGuess guessResults =
    List.filter (matchesPreviousGuesses guessResults) commonWords |> List.head |> Maybe.withDefault "error"


matchesPreviousGuesses : Dict Char LetterGuessResult -> String -> Bool
matchesPreviousGuesses guessResults w =
    let
        word =
            String.toUpper w

        letterIndexPairs =
            String.toList word |> List.indexedMap Tuple.pair

        wordArr =
            String.toList word |> Array.fromList

        doesntContainKnownBad ( i, c ) =
            case Dict.get c guessResults of
                Nothing ->
                    True

                Just NotInWord ->
                    False

                Just (InWord _ knownBadIndicies) ->
                    if Set.member i knownBadIndicies then
                        False

                    else
                        True

        knownGood =
            Dict.toList guessResults
                |> List.filterMap
                    (\( letter, letterResults ) ->
                        case letterResults of
                            InWord knownGoodIndicies _ ->
                                Just
                                    ( letter
                                    , if Set.isEmpty knownGoodIndicies then
                                        Nothing

                                      else
                                        Just knownGoodIndicies
                                    )

                            _ ->
                                Nothing
                    )

        matchesWord ( expectedLetter, maybeIndicies ) =
            case maybeIndicies of
                Just indicies ->
                    List.all
                        (\i ->
                            case Array.get i wordArr of
                                Nothing ->
                                    False

                                Just actualLetter ->
                                    expectedLetter == actualLetter
                        )
                        (Set.toList indicies)

                Nothing ->
                    String.contains (String.fromChar expectedLetter) word
    in
    List.all doesntContainKnownBad letterIndexPairs && List.all matchesWord knownGood
