module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes as Html
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
      }
    , Task.perform GotTime Time.now
    )


update msg model =
    case msg of
        GotTime time ->
            ( { model | time = Just time }, Cmd.none )

        Start activity ->
            -- TODO: Save running activity to log
            ( { model
                | activity =
                    case model.time of
                        Nothing ->
                            Nothing

                        Just time ->
                            Just { name = activity, startedAt = time }
              }
            , Cmd.none
            )

        Stop ->
            -- TODO: Save running activity to log
            ( { model | activity = Nothing }, Cmd.none )



--    ( model, Cmd.none )


subscriptions model =
    Sub.none


type alias Model =
    { log : LogOfTheDay
    , activity : Maybe Activity
    , time : Maybe Time.Posix
    }


type Msg
    = GotTime Time.Posix
    | Start ActivityName
    | Stop


type alias Activity =
    { name : ActivityName
    , startedAt : Time.Posix
    }


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
    String


type alias Hours =
    Float


view model =
    { title = "My time"
    , body =
        [ Html.main_ [ Html.style "margin" "1em" ]
            [ viewLogByTime model.log
            , viewLog model.log
            , viewStartableActivities model.log
            , Html.section [ Html.style "margin" "1em" ]
                [ Html.input
                    [ Html.style "width" "100%"
                    , Html.style "max-width" "20em"
                    , Html.placeholder "add other activity"
                    ]
                    []
                ]
            , Html.section [ Html.style "margin" "1em" ]
                [ Html.button [] [ Html.text "Stop" ] ]
            ]
        ]
    }


logOfTheDayExample =
    [ ( "lunch", Dict.fromList [ ( "12:00", 1 ) ] )
    , ( "break", Dict.empty )
    , ( "mail", Dict.empty )
    , ( "sanitary", Dict.empty )
    , ( "write report", Dict.fromList [ ( "10:00", 2 ), ( "13:00", 1 ) ] )
    ]
        |> Dict.fromList


viewLogByTime activities =
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
        |> List.map viewTimeEntry
        |> Html.section [ Html.style "margin" "1em" ]


viewTimeEntry ( time, activity, hours ) =
    Html.article [] [ Html.text <| time ++ " - " ++ String.fromFloat hours ++ "h " ++ activity ]


viewLog activities =
    activities
        |> Dict.toList
        |> List.map viewLogEntriesForActivity
        |> Html.section [ Html.style "margin" "1em" ]


viewLogEntriesForActivity ( activity, entries ) =
    entries
        |> Dict.toList
        |> List.map viewLogEntries
        |> Html.dd []
        |> List.singleton
        |> (::) (Html.dt [] [ Html.text activity ])
        |> Html.dl []


viewLogEntries ( time, hours ) =
    Html.dd []
        [ Html.text (time ++ " - " ++ String.fromFloat hours ++ " uur") ]


viewStartableActivities activities =
    activities
        |> Dict.keys
        |> List.map viewStartableActivity
        |> Html.section [ Html.style "margin" "1em" ]


viewStartableActivity activity =
    Html.article
        [ Html.style "display" "flex"
        , Html.style "width" "100%"
        , Html.style "max-width" "20em"
        ]
        [ Html.span [ Html.style "flex" "1" ] [ Html.text activity ]
        , Html.button [] [ Html.text "Start" ]
        ]
