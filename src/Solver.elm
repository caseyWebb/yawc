module Solver exposing (generateNextGuess)

import Array
import Dict exposing (Dict)
import LetterGuessResult exposing (LetterGuessResult(..))
import Random
import Set
import Words exposing (commonWords)


generateNextGuess : (String -> msg) -> String -> Dict Char LetterGuessResult -> Cmd msg
generateNextGuess mapMsg winningWord guessResults =
    let
        possibleWords =
            List.filter (matchesPreviousGuesses guessResults) commonWords
                |> List.map
                    (\word ->
                        ( if word == winningWord then
                            0

                          else
                            scoreWord word
                        , word
                        )
                    )

        ( head, tail ) =
            case possibleWords of
                x :: xs ->
                    ( x, xs )

                [] ->
                    ( ( 1, "ERROR" ), [] )
    in
    Random.weighted head tail |> Random.generate mapMsg


scoreWord : String -> Float
scoreWord word =
    let
        -- https://www3.nd.edu/~busiforc/handouts/cryptography/letterfrequencies.html
        letterFreq =
            Dict.fromList
                [ ( 'E', 57 )
                , ( 'A', 43 )
                , ( 'R', 39 )
                , ( 'I', 38 )
                , ( 'O', 37 )
                , ( 'T', 35 )
                , ( 'N', 34 )
                , ( 'S', 29 )
                , ( 'L', 28 )
                , ( 'C', 23 )
                , ( 'U', 19 )
                , ( 'D', 17 )
                , ( 'P', 16 )
                , ( 'M', 15 )
                , ( 'H', 15 )
                , ( 'G', 13 )
                , ( 'B', 11 )
                , ( 'F', 9 )
                , ( 'Y', 9 )
                , ( 'W', 7 )
                , ( 'K', 6 )
                , ( 'V', 5 )
                , ( 'X', 1 )
                , ( 'Z', 1 )
                , ( 'J', 1 )
                , ( 'Q', 1 )
                ]

        uniqChars =
            word |> String.toList |> Set.fromList |> Set.toList
    in
    List.filterMap (\char -> Dict.get char letterFreq) uniqChars |> List.product


matchesPreviousGuesses : Dict Char LetterGuessResult -> String -> Bool
matchesPreviousGuesses guessResults word =
    let
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
