module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Parser exposing ((|.), (|=))


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { csv : String
    , schedule : Result String (List Event)
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
    ( { csv = "", schedule = Ok [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Csv csv ->
            ( { model | csv = csv }, Cmd.none )

        GenerateSchedule ->
            ( { model | schedule = generate (String.split "\n" model.csv) }, Cmd.none )


generate : List String -> Result String (List Event)
generate rows =
    generateHelper rows []


generateHelper : List String -> List Event -> Result String (List Event)
generateHelper rows events =
    case rows of
        [] ->
            Ok events

        row :: rest ->
            toEvent row |> Result.andThen (\event -> generateHelper rest (event :: events))


toEvent : String -> Result String Event
toEvent row =
    case String.split "," row of
        [ name, start, end ] ->
            case ( toTime start, toTime end ) of
                ( Ok s, Ok e ) ->
                    Ok (Event name s e)

                _ ->
                    Err "I wasn't able to convert the values to times"

        _ ->
            Err <| "I was expecting three comma separated values but got " ++ row ++ " instead"


toTime : String -> Result (List Parser.DeadEnd) Time
toTime time =
    String.replace " " "" time
        |> String.toUpper
        |> Parser.run timeParser


timeParser : Parser.Parser Time
timeParser =
    Parser.succeed Time
        |= timeNumeralParser
        |. Parser.symbol ":"
        |= timeNumeralParser
        |. Parser.oneOf [ Parser.keyword "AM", Parser.keyword "PM" ]


timeNumeralParser : Parser.Parser Int
timeNumeralParser =
    Parser.oneOf
        [ Parser.int
        , Parser.token "0"
            |> Parser.andThen (\_ -> Parser.int)
        ]


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


viewSchedule : Result String (List Event) -> Html Msg
viewSchedule events =
    case events of
        Err error ->
            text <| "There was an error generating the schedule: " ++ error

        Ok es ->
            div [] (List.map viewEvent es)


viewEvent : Event -> Html Msg
viewEvent (Event name start end) =
    div [] [ text name, viewTime start, viewTime end ]


viewTime : Time -> Html Msg
viewTime (Time hour minute) =
    text <| String.fromInt hour ++ String.fromInt minute
