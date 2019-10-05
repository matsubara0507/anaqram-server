module AnaQRam.Generated.API exposing (Score, getApiProblem, getApiScores, getApiSizes, jsonDecScore, jsonEncScore, maybeBoolToIntStr, postApiScores)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
    case mx of
        Nothing ->
            ""

        Just True ->
            "1"

        Just False ->
            "0"


type alias Score =
    { textLength : Int
    , clearTime : Int
    , swapCount : Int
    }


jsonDecScore : Json.Decode.Decoder Score
jsonDecScore =
    Json.Decode.succeed (\ptextLength pclearTime pswapCount -> { textLength = ptextLength, clearTime = pclearTime, swapCount = pswapCount })
        |> required "textLength" Json.Decode.int
        |> required "clearTime" Json.Decode.int
        |> required "swapCount" Json.Decode.int


jsonEncScore : Score -> Value
jsonEncScore val =
    Json.Encode.object
        [ ( "textLength", Json.Encode.int val.textLength )
        , ( "clearTime", Json.Encode.int val.clearTime )
        , ( "swapCount", Json.Encode.int val.swapCount )
        ]


getApiSizes : (Result Http.Error (List Int) -> msg) -> Cmd msg
getApiSizes toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "sizes"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list Json.Decode.int)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiProblem : Int -> (Result Http.Error String -> msg) -> Cmd msg
getApiProblem query_size toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ Just query_size
                            |> Maybe.map (String.fromInt >> Url.Builder.string "size")
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "problem"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg Json.Decode.string
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiScores : (Result Http.Error (List Score) -> msg) -> Cmd msg
getApiScores toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "scores"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecScore)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiScores : Score -> (Result Http.Error Score -> msg) -> Cmd msg
postApiScores body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "scores"
                ]
                params
        , body =
            Http.jsonBody (jsonEncScore body)
        , expect =
            Http.expectJson toMsg jsonDecScore
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
