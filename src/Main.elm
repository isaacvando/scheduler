module Main exposing (..)

import Browser
import Dict
import Dict.Extra as Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Parser exposing ((|.), (|=), int)


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
    ( { csv =
            String.join "\n"
                [ "How to Grow a Flavorful Tomato,2:00PM,2:55PM,Room A"
                , "The Effects of Excessive Tomato Consumption,3:00PM,3:45PM,Room B"
                , "I love waking up early,11:00AM,1:25PM,Room C"
                , "Another one,12:00AM,1:25PM,Room B"
                , "Yet Another,11:05AM,1:00PM,Room A"
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
            [ style "font-face" "sans-serif"
            , style "margin-left" "20px"
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
            , placeholder "How to Grow a Flavorful Tomato,2:00PM,2:55PM\nThe Effects of Excessive Tomato Consumption,3:00PM,3:45PM"
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
                        |> List.map (\(Event _ start _ _) -> toMinutes start)
                        |> List.minimum
                        |> Maybe.withDefault 0
            in
            div
                [ style "border" "1px solid black"
                , style "border-radius" "10"
                , class "schedule"
                , style "display" "grid"
                , style "grid-template-columns" (List.map (const width) (groupByVenue es) |> String.join " ")
                , style "gap" "14px"
                , style "padding" "7px"
                , style "position" "relative"
                ]
                (List.map (viewColumn startTime (getTotalTime es)) (groupByVenue es))


getTotalTime : List Event -> Int
getTotalTime events =
    (events
        |> List.map (\(Event _ _ end _) -> toMinutes end)
        |> List.maximum
        |> Maybe.withDefault 0
    )
        - (events
            |> List.map (\(Event _ start _ _) -> toMinutes start)
            |> List.minimum
            |> Maybe.withDefault 0
          )
        |> scale


groupByVenue : List Event -> List ( String, List Event )
groupByVenue events =
    events
        |> Dict.groupBy (\(Event _ _ _ venue) -> venue)
        |> Dict.toList


viewColumn : Int -> Int -> ( String, List Event ) -> Html Msg
viewColumn startTime totalTime ( title, events ) =
    div
        [ class "column"
        ]
        [ text title
        , hr
            [ style "background-color" "black"
            , style "border-style" "solid"
            , style "border-width" "0.5px"
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
viewEvent startTime (Event name start end _) =
    div
        [ style "width" width
        , style "box-sizing" "border-box"
        , style "background-color" "#ADD8E6"
        , style "padding" "5px"
        , style "border" "1px solid black"
        , style "border-radius" "5px"
        , style "font-size" "12px"
        , style "position" "absolute"
        , style "display" "grid"
        , style "place-items" "center"
        , toMinutes start - startTime |> scale |> String.fromInt |> style "top"
        , toMinutes end - toMinutes start |> scale |> String.fromInt |> style "height"
        ]
        [ text name
        , br [] []
        , text <| viewTime start ++ " - " ++ viewTime end
        ]


scale : Int -> Int
scale x =
    toFloat x * 1.4 |> ceiling


width : String
width =
    "140px"


toMinutes : Time -> Int
toMinutes (Time hour minute amPm) =
    60
        * hour
        + minute
        + (if amPm == PM then
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
