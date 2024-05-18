module Main exposing (main)

import Dict exposing (Dict)
import Html
import Html.Attributes as Html


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


main =
    Html.main_ [ Html.style "margin" "1em" ]
        [ viewLogByTime logOfTheDayExample
        , viewLog logOfTheDayExample
        , viewStartableActivities logOfTheDayExample
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
