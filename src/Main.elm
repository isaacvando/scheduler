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
    = Time Int Int AmPm


type AmPm
    = AM
    | PM


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = "How to Grow a Flavorful Tomato,2:00PM,2:55PM\nThe Effects of Excessive Tomato Consumption,3:00PM,3:45PM", schedule = Ok [] }, Cmd.none )


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
            toTime start
                |> Result.andThen (\s -> toTime end |> Result.andThen (\e -> Ok (Event name s e)))
                |> Result.mapError (\_ -> "I was trying to parse a time value for " ++ name ++ " but I got stuck.")

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
            div [] (List.map viewEvent es)


viewEvent : Event -> Html Msg
viewEvent (Event name start end) =
    div
        [ style "width" "120"
        , style "height" "120"
        , style "background-color" "#ADD8E6"
        , style "display" "inline-block"
        , style "margin" "10"
        , style "padding" "5"
        , style "border" "1px solid black"
        , style "border-radius" "5px"
        , style "font-size" "12px"
        ]
        [ p [] [ text name ]
        , p [] [ viewTime start ]
        , p [] [ viewTime end ]
        ]


viewTime : Time -> Html Msg
viewTime (Time hour minute amPm) =
    text <| String.fromInt hour ++ String.fromInt minute ++ fromString amPm


fromString : AmPm -> String
fromString amPm =
    case amPm of
        AM ->
            "AM"

        PM ->
            "PM"
