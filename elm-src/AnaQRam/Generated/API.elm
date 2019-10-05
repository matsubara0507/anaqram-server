module AnaQRam.Generated.API exposing (getApiProblem, getApiSizes, maybeBoolToIntStr)

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
