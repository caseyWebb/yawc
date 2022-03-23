module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    {}


type alias Model =
    { name : String
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "Casey" }, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "YAWC"
    , body =
        [ h1 [] [ text ("Hello, " ++ model.name) ]
        ]
    }
