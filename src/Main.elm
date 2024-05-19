module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Task
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { log = logOfTheDayExample
      , activity = Nothing
      , time = Nothing
      , timezone = Nothing
      }
    , Task.perform GotTimeZone Time.here
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeAfter runningActivity time ->
            let
                log =
                    logActivity runningActivity time model.time model.log
            in
            ( { model | time = Just time, log = log }, Cmd.none )

        GotTimeZone zone ->
            ( { model | timezone = Just zone }, Cmd.none )

        Start activity ->
            ( { model | activity = Just activity }, Task.perform (GotTimeAfter model.activity) Time.now )

        Stop ->
            ( { model | activity = Nothing }, Task.perform (GotTimeAfter model.activity) Time.now )


logActivity : Maybe ActivityName -> Time.Posix -> Maybe Time.Posix -> LogOfTheDay -> LogOfTheDay
logActivity activity stoppedAt maybeStartedAt log =
    case maybeStartedAt of
        Nothing ->
            log

        Just startedAt ->
            case activity of
                Nothing ->
                    log

                Just name ->
                    log
                        |> Dict.update name
                            (\entries ->
                                entries
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.insert (Time.posixToMillis startedAt) (toFloat (Time.posixToMillis stoppedAt - Time.posixToMillis startedAt) / oneHourInMillis)
                                    |> Just
                            )


oneHourInMillis =
    3600000


subscriptions model =
    Sub.none


type alias Model =
    { log : LogOfTheDay
    , activity : Maybe ActivityName
    , time : Maybe Time.Posix
    , timezone : Maybe Time.Zone
    }


type Msg
    = GotTimeAfter (Maybe ActivityName) Time.Posix
    | GotTimeZone Time.Zone
    | Start ActivityName
    | Stop


type alias Log =
    Dict LogDay LogOfTheDay


type alias LogDay =
    String


type alias LogOfTheDay =
    Dict ActivityName LogEntries


type alias ActivityName =
    String


type alias LogEntries =
    Dict LogTime Hours


type alias LogTime =
    -- millis from posix time
    Int


type alias Hours =
    Float


view : Model -> Browser.Document Msg
view model =
    { title = "My time"
    , body =
        [ Html.main_ [ Html.style "margin" "1em" ]
            (case model.timezone of
                Nothing ->
                    [ Html.text "timezone not yet known" ]

                Just timezone ->
                    [ viewLogByTime timezone model.log
                    , viewLog timezone model.log
                    , viewStartableActivities model.activity model.log
                    , Html.section [ Html.style "margin" "1em" ]
                        [ Html.input
                            [ Html.style "width" "100%"
                            , Html.style "max-width" "20em"
                            , Html.placeholder "add other activity"
                            ]
                            []
                        ]
                    , Html.section [ Html.style "margin" "1em" ]
                        [ Html.button [ Html.onClick Stop ] [ Html.text "Stop" ] ]
                    ]
            )
        ]
    }


logOfTheDayExample : LogOfTheDay
logOfTheDayExample =
    [ ( "lunch", Dict.empty )
    , ( "break", Dict.empty )
    , ( "mail", Dict.empty )
    , ( "sanitary", Dict.empty )
    , ( "write report", Dict.empty )
    ]
        |> Dict.fromList


viewLogByTime : Time.Zone -> LogOfTheDay -> Html Msg
viewLogByTime timezone activities =
    activities
        |> Dict.toList
        |> List.map
            (\( activity, entries ) ->
                entries
                    |> Dict.toList
                    |> List.map (\( time, hours ) -> ( time, activity, hours ))
            )
        |> List.concat
        |> List.sortBy (\( a, b, c ) -> a)
        |> List.map (viewTimeEntry timezone)
        |> Html.section [ Html.style "margin" "1em" ]


viewTimeEntry timezone ( time, activity, hours ) =
    Html.article [] [ Html.text <| viewTime timezone time ++ " - " ++ String.fromFloat hours ++ "h " ++ activity ]


viewLog : Time.Zone -> LogOfTheDay -> Html Msg
viewLog timezone activities =
    activities
        |> Dict.toList
        |> List.map (viewLogEntriesForActivity timezone)
        |> Html.section [ Html.style "margin" "1em" ]


viewLogEntriesForActivity timezone ( activity, entries ) =
    entries
        |> Dict.toList
        |> List.map (viewLogEntries timezone)
        |> Html.dd []
        |> List.singleton
        |> (::) (Html.dt [] [ Html.text activity ])
        |> Html.dl []


viewLogEntries timezone ( time, hours ) =
    Html.dd []
        [ Html.text (viewTime timezone time ++ " - " ++ String.fromFloat hours ++ " uur") ]


viewTime zone time =
    let
        posix =
            Time.millisToPosix time
    in
    (Time.toHour zone posix
        |> String.fromInt
        |> String.pad 2 '0'
    )
        ++ ":"
        ++ (Time.toMinute zone posix
                |> String.fromInt
                |> String.pad 2 '0'
           )


viewStartableActivities runningActivity activities =
    activities
        |> Dict.keys
        |> List.map (viewStartableActivity runningActivity)
        |> Html.section [ Html.style "margin" "1em" ]


viewStartableActivity runningActivity activity =
    Html.article
        [ Html.style "display" "flex"
        , Html.style "width" "100%"
        , Html.style "max-width" "20em"
        ]
        [ Html.span [ Html.style "flex" "1" ] [ Html.text activity ]
        , Html.button [ Html.disabled (runningActivity == Just activity), Html.onClick (Start activity) ] [ Html.text "Start" ]
        ]
