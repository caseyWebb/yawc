module Keyboard exposing (Msg(..), view)

import Colors as Colors
import Css as Css
import Dict as Dict exposing (Dict)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import LetterGuessResult exposing (LetterGuessResult(..))
import Set as Set


type Msg
    = KeyClicked Char


type alias Model =
    { pastGuessResults : Dict Char LetterGuessResult
    }


view : Model -> (Msg -> msg) -> Html msg
view model clickMsg =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        ([ "QWERTYUIOP", "ASDFGHJKL", "ZXCVBNM" ] |> List.map (viewRow model clickMsg))


viewRow : Model -> (Msg -> msg) -> String -> Html msg
viewRow model clickMsg row =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.center
            ]
        ]
        (List.map (viewButton model clickMsg) (String.toList row))


viewButton : Model -> (Msg -> msg) -> Char -> Html msg
viewButton model mapMsg letter =
    button
        [ css
            [ Dict.get letter model.pastGuessResults
                |> Maybe.map
                    (\state ->
                        case state of
                            InWord knownPositions _ ->
                                if Set.isEmpty knownPositions then
                                    Colors.yellow

                                else
                                    Colors.green

                            NotInWord ->
                                Colors.gray
                    )
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
        , onClick (KeyClicked letter |> mapMsg)
        ]
        [ text <| String.fromChar letter ]
