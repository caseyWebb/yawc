module Toast exposing (..)

import Css
import Css.Transitions as Transitions
import Html.Styled exposing (Html, text)
import Html.Styled.Attributes exposing (css)
import Process
import Task


type Msg
    = Show String (Maybe Int)
    | Dismiss Int


type alias Model =
    { message : String
    , messageId : Int
    , show : Bool
    }


show : (Msg -> msg) -> String -> Maybe Int -> Cmd msg
show toMsg message maybeAutoDismiss =
    Task.perform (\_ -> Show message maybeAutoDismiss |> toMsg) (Task.succeed ())


init : Model
init =
    { message = ""
    , messageId = 0
    , show = False
    }


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toMsg msg model =
    case msg of
        Show message maybeAutoDismiss ->
            let
                messageId =
                    model.messageId + 1

                dismiss =
                    case maybeAutoDismiss of
                        Just ms ->
                            ms
                                |> toFloat
                                |> Process.sleep
                                |> Task.perform (\_ -> Dismiss messageId |> toMsg)

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | message = message
                , messageId = messageId
                , show = True
              }
            , dismiss
            )

        Dismiss messageId ->
            if messageId == model.messageId then
                ( { model | show = False }, Cmd.none )

            else
                ( model, Cmd.none )


view : Model -> Html msg
view model =
    let
        ( opacity, transitionDuration ) =
            if model.show then
                ( 1, 0 )

            else
                ( 0, 300 )
    in
    Html.Styled.div
        [ css
            [ Css.backgroundColor (Css.hex "fff")
            , Css.color (Css.hex "000")
            , Css.fontSize (Css.px 18)
            , Css.fontWeight Css.bold
            , Css.padding (Css.px 20)
            , Css.borderRadius (Css.px 4)
            , Css.opacity (Css.int opacity)
            , Transitions.transition
                [ Transitions.opacity transitionDuration ]
            ]
        ]
        [ text model.message ]
