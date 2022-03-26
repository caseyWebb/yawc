module Toast exposing (..)

import Css
import Css.Transitions as Transitions
import Html.Styled exposing (Html, text)
import Html.Styled.Attributes exposing (css)
import Process
import Task exposing (Task)


type Msg
    = Show String (Maybe Float)
    | Dismiss Int


type alias Model =
    { message : String
    , messageId : Int
    , show : Bool
    }


show : String -> Maybe Int -> Task msg Msg
show message maybeAutoDismiss =
    Task.succeed (Show message (Maybe.map toFloat maybeAutoDismiss))


init : Model
init =
    { message = ""
    , messageId = 0
    , show = False
    }


update : Msg -> Model -> ( Model, Maybe (Task msg Msg) )
update msg model =
    case msg of
        Show message maybeAutoDismiss ->
            let
                messageId =
                    model.messageId + 1

                dismiss =
                    Maybe.map
                        (Process.sleep
                            >> Task.andThen
                                (\_ ->
                                    Task.succeed (Dismiss messageId)
                                )
                        )
                        maybeAutoDismiss
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
                ( { model | show = False }, Nothing )

            else
                ( model, Nothing )


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
