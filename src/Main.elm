module Main exposing (..)

import Browser
import Dict
import Dict.Extra as Dict
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
    = Event String Time Time String


type Time
    = Time Int Int AmPm


type AmPm
    = AM
    | PM


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = "How to Grow a Flavorful Tomato,2:00PM,2:55PM,Room A\nThe Effects of Excessive Tomato Consumption,3:00PM,3:45PM,Room B", schedule = Ok [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Csv csv ->
            ( { model | csv = csv }, Cmd.none )

        GenerateSchedule ->
            ( { model | schedule = generate model.csv }, Cmd.none )


generate : String -> Result String (List Event)
generate input =
    String.lines input
        |> generateHelper []


generateHelper : List Event -> List String -> Result String (List Event)
generateHelper events rows =
    case rows of
        [] ->
            Ok events

        row :: rest ->
            toEvent row |> Result.andThen (\event -> generateHelper (event :: events) rest)


toEvent : String -> Result String Event
toEvent row =
    case String.split "," row of
        [ name, start, end, venue ] ->
            toTime start
                |> Result.andThen (\s -> toTime end |> Result.andThen (\e -> Ok (Event name s e venue)))

        _ ->
            Err <| "I was expecting four comma separated values but got " ++ row ++ " instead"


toTime : String -> Result String Time
toTime time =
    time
        |> String.replace " " ""
        |> String.toUpper
        |> Parser.run timeParser
        |> Result.mapError (\_ -> "I was trying to parse a time value for the input'" ++ time ++ "' but I got stuck.")


timeParser : Parser.Parser Time
timeParser =
    Parser.succeed Time
        |= timeNumeralParser
        |. Parser.symbol ":"
        |= timeNumeralParser
        |= amPmParser


amPmParser : Parser.Parser AmPm
amPmParser =
    Parser.oneOf [ Parser.map (\_ -> AM) (Parser.symbol "AM"), Parser.map (\_ -> PM) (Parser.symbol "PM") ]


timeNumeralParser : Parser.Parser Int
timeNumeralParser =
    Parser.oneOf
        [ Parser.token "0"
            |> Parser.andThen (\_ -> Parser.int)
        , Parser.int
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Schedule Maker"
    , body =
        [ div
            [ class "container"
            , style "font-face" "sans-serif"
            , style "width" "70%"
            , style "margin" "auto"
            ]
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
                    , value model.csv
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
            let
                grouped =
                    groupByVenue es
            in
            table
                [ style "height" "480"
                , style "border" "1px solid black"
                , style "border-radius" "10"
                ]
                [ thead [] (grouped |> List.map (\( venue, _ ) -> th [] [ text venue ]))
                , tr [] (grouped |> List.map (\( _, xs ) -> td [] [ div [] (List.map viewEvent xs) ]))
                ]



-- (groupByVenue es |> List.map viewVenue)


groupByVenue : List Event -> List ( String, List Event )
groupByVenue events =
    events
        |> Dict.groupBy (\(Event _ _ _ venue) -> venue)
        |> Dict.toList


viewVenue : ( String, List Event ) -> Html Msg
viewVenue ( venue, events ) =
    div
        [ style "width" "100%"
        , style "margin" "10"
        ]
        [ th [] [ text venue ]
        , div
            [ style "width" "100%"
            , style "margin" "10"
            ]
            (events |> List.map viewEvent)
        ]


viewEvent : Event -> Html Msg
viewEvent (Event name start end venue) =
    div
        [ style "width" "120"
        , style "height" "100"
        , style "background-color" "#ADD8E6"
        , style "display" "inline-block"
        , style "margin" "10"
        , style "padding" "2"
        , style "border" "1px solid black"
        , style "border-radius" "5px"
        , style "font-size" "12px"
        ]
        [ p [] [ text name ]
        , p [] [ text <| viewTime start ++ " - " ++ viewTime end ]
        ]


viewTime : Time -> String
viewTime (Time hour minute amPm) =
    String.fromInt hour ++ ":" ++ viewTimeNumeral minute ++ " " ++ fromString amPm


viewTimeNumeral : Int -> String
viewTimeNumeral n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


fromString : AmPm -> String
fromString amPm =
    case amPm of
        AM ->
            "AM"

        PM ->
            "PM"
