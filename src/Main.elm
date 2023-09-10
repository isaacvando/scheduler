module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { csv : String
    , schedule : Maybe (List Event)
    }


type Msg
    = NoOp
    | GenerateSchedule
    | Csv String


type Event
    = Event String Time Time


type Time
    = Time Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = "", schedule = Just [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Csv csv ->
            ( { model | csv = csv }, Cmd.none )

        GenerateSchedule ->
            ( { model | schedule = generate (String.split "\n" model.csv) }, Cmd.none )


generate : List String -> Maybe (List Event)
generate rows =
    generateHelper rows []


generateHelper : List String -> List Event -> Maybe (List Event)
generateHelper rows events =
    case rows of
        [] ->
            Just events

        row :: rest ->
            toEvent row |> Maybe.andThen (\event -> generateHelper rest (event :: events))


toEvent : String -> Maybe Event
toEvent row =
    case String.split "," row of
        [ name, start, end ] ->
            case ( toTime start, toTime end ) of
                ( Just s, Just e ) ->
                    Just (Event name s e)

                _ ->
                    Nothing

        _ ->
            Nothing


toTime : String -> Maybe Time
toTime time =
    Nothing


view : Model -> Browser.Document Msg
view model =
    { title = "Schedule Maker"
    , body =
        [ div [ class "container" ]
            [ h1 [] [ text "Schedule Maker" ]
            , viewSchedule model.schedule
            , div [ class "form-row" ]
                [ label [] [ text "Event Schedule" ]
                , textarea
                    [ rows 15
                    , cols 70
                    , style "display" "block"
                    , placeholder "How to Grow a Flavorful Tomato,2:00PM,2:55PM\nThe Effects of Excessive Tomato Consumption,3:00PM,3:45PM"
                    , onInput Csv
                    ]
                    []
                ]
            , button [ onClick GenerateSchedule ] [ text "Generate Schedule" ]
            ]
        ]
    }


viewSchedule : Maybe (List Event) -> Html Msg
viewSchedule events =
    case events of
        Nothing ->
            text "There was an error generating the schedule. Please confirm your input is formatted properly."

        Just es ->
            text "your schedule"
