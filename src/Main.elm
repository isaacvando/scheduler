module Main exposing (..)

import Browser
import Csv.Decode as Csv exposing (Decoder)
import Dict
import Dict.Extra as Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Parser exposing ((|.), (|=), int)


main : Program () Model Msg
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


type alias Event =
    { title : String
    , name : String
    , start : Time
    , end : Time
    , venue : String
    , link : String
    }



-- EventRow is used to represent a row of the CSV file. It would be nice to decode the CSV directly into the Event type
-- but I'm not sure of a good way to do this while preserving error messages


type alias EventRow =
    { title : String
    , name : String
    , start : String
    , end : String
    , venue : String
    , link : String
    }


type Time
    = Time Int Int AmPm


type AmPm
    = AM
    | PM


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv =
            String.join "\n"
                [ "title,name,start,end,venue,link"
                , "How to Grow a Flavorful Tomato,jeff bob,2:00PM,2:55PM,Room A,https://example.com"
                , "The Effects of Excessive Tomato Consumption,jeff bob,3:00PM,3:45PM,Room B,https://example.com"
                , "I love waking up early,jeff bob,11:00AM,1:25PM,Room C,https://example.com"
                , "Another one,jeff bob,12:00AM,1:25PM,Room B,https://example.com"
                ]
      , schedule = Ok []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Csv csv ->
            ( { model | csv = csv }, Cmd.none )

        GenerateSchedule ->
            ( { model | schedule = parseRows model.csv }, Cmd.none )


eventDecoder : Decoder EventRow
eventDecoder =
    Csv.into EventRow
        |> Csv.pipeline (Csv.field "title" Csv.string)
        |> Csv.pipeline (Csv.field "name" Csv.string)
        |> Csv.pipeline (Csv.field "start" Csv.string)
        |> Csv.pipeline (Csv.field "end" Csv.string)
        |> Csv.pipeline (Csv.field "venue" Csv.string)
        |> Csv.pipeline (Csv.field "link" Csv.string)


parseRows : String -> Result String (List Event)
parseRows rows =
    rows
        |> Csv.decodeCsv Csv.FieldNamesFromFirstRow eventDecoder
        |> Result.mapError Csv.errorToString
        |> Result.andThen (process [])


process : List Event -> List EventRow -> Result String (List Event)
process events rows =
    case rows of
        [] ->
            Ok events

        r :: rs ->
            eventRowToRow r |> Result.andThen (\e -> process (e :: events) rs)


eventRowToRow : EventRow -> Result String Event
eventRowToRow row =
    let
        x =
            Debug.log "event row" row
    in
    toTime row.start
        |> Result.andThen
            (\s ->
                toTime row.end
                    |> Result.andThen
                        (\e ->
                            Ok
                                { title = row.title
                                , name = row.name
                                , start = s
                                , end = e
                                , venue = row.venue
                                , link = row.link
                                }
                        )
            )


toTime : String -> Result String Time
toTime time =
    time
        |> String.replace " " ""
        |> String.toUpper
        |> Parser.run timeParser
        |> Result.mapError (\_ -> "I was trying to parse a time value for the input '" ++ time ++ "' but I got stuck.")


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
            [ style "font-face" "sans-serif"
            , style "display" "flex"
            , style "align-items" "center"
            , style "flex-direction" "column"
            ]
            [ h1 [] [ text "Schedule Maker" ]
            , viewSchedule model.schedule
            , viewForm model
            ]
        ]
    }


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ label [] [ text "Event Schedule" ]
        , textarea
            [ rows 15
            , cols 70
            , style "display" "block"
            , placeholder "How to Grow a Flavorful Tomato,2:00PM,2:55PM,Room A,https://example.com\nThe Effects of Excessive Tomato Consumption,3:00PM,3:45PM,Room A,https://example.com"
            , onInput Csv
            , value model.csv
            ]
            []
        , button [ onClick GenerateSchedule ] [ text "Generate Schedule" ]
        ]


viewSchedule : Result String (List Event) -> Html Msg
viewSchedule events =
    case events of
        Err error ->
            text <| "There was an error generating the schedule: " ++ error

        Ok es ->
            let
                startTime =
                    es
                        |> List.map (\e -> toMinutes e.start)
                        |> List.minimum
                        |> Maybe.withDefault 0

                grouped =
                    es
                        |> Dict.groupBy .venue
                        |> Dict.toList
            in
            div
                [ class "schedule"
                , List.map (const width) grouped
                    |> String.join " "
                    |> style "grid-template-columns"
                ]
                (List.map (viewColumn startTime (getTotalTime es)) grouped)


getTotalTime : List Event -> Int
getTotalTime events =
    (events
        |> List.map (\e -> toMinutes e.end)
        |> List.maximum
        |> Maybe.withDefault 0
    )
        - (events
            |> List.map (\e -> toMinutes e.start)
            |> List.minimum
            |> Maybe.withDefault 0
          )
        |> scale


viewColumn : Int -> Int -> ( String, List Event ) -> Html Msg
viewColumn startTime totalTime ( title, events ) =
    div
        [ class "column"
        ]
        [ text title
        , hr
            [ class "rule"
            ]
            []
        , viewEvents startTime totalTime events
        ]


viewEvents : Int -> Int -> List Event -> Html Msg
viewEvents startTime totalTime events =
    div
        [ style "position" "relative"
        , style "height" (String.fromInt totalTime ++ "px")
        ]
        (List.map (viewEvent startTime) events)


viewEvent : Int -> Event -> Html Msg
viewEvent startTime event =
    a
        [ class "event"
        , style "width" width
        , toMinutes event.start - startTime |> scale |> String.fromInt |> style "top"
        , toMinutes event.end - toMinutes event.start |> scale |> String.fromInt |> style "height"
        , (if event.link == "" then
            "#"

           else
            event.link
          )
            |> href
        ]
        [ div []
            [ i [ style "margin" "0px" ] [ text <| event.title ]
            , text <| " - " ++ event.name
            , br [] []
            , text <| viewTime event.start ++ " - " ++ viewTime event.end
            ]
        ]


scale : Int -> Int
scale x =
    let
        y =
            toFloat x * 2.5 |> ceiling
    in
    if modBy 2 y == 0 then
        y

    else
        y + 1


width : String
width =
    "165px"


toMinutes : Time -> Int
toMinutes (Time hour minute amPm) =
    60
        * hour
        + minute
        + (if amPm == PM && hour /= 12 then
            12 * 60

           else
            0
          )


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


const : a -> (b -> a)
const result =
    \_ -> result
