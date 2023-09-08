module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    {}


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Schedule Maker"
    , body =
        [ div [ class "container" ]
            [ h1 [] [ text "Schedule Maker" ] ]
        ]
    }
