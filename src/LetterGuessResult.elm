module LetterGuessResult exposing (..)

import Set exposing (Set)


type LetterGuessResult
    = InWord (Set Int) (Set Int) -- InWord atIndicies notAtIndicies
    | NotInWord
