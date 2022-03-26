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
    = Letter Char
    | Backspace
    | Enter


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
        ([ "QWERTYUIOP", "ASDFGHJKL", "⇦ZXCVBNM⏎" ] |> List.map (viewRow model clickMsg))


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
viewButton model toMsg letter =
    let
        isBackspace =
            letter == '⇦'

        isEnter =
            letter == '⏎'

        ( width, fontSize ) =
            if isBackspace || isEnter then
                ( 60, 1.5 )

            else
                ( 35, 1 )
    in
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
            , Css.width (Css.px width)
            , Css.margin (Css.px 3)
            , Css.fontSize (Css.rem fontSize)
            , Css.lineHeight (Css.rem fontSize)
            , Css.fontWeight Css.bold
            , Css.borderRadius (Css.px 4)
            , Css.borderStyle Css.none
            , Css.cursor Css.pointer
            ]
        , onClick
            ((if isBackspace then
                Backspace

              else if isEnter then
                Enter

              else
                Letter letter
             )
                |> toMsg
            )
        ]
        [ text <| String.fromChar letter ]
