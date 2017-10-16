module Generated.ScoreAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Score =
    { textLength : Int
    , clearTime : Int
    , swapCount : Int
    }

decodeScore : Decoder Score
decodeScore =
    decode Score
        |> required "textLength" int
        |> required "clearTime" int
        |> required "swapCount" int

encodeScore : Score -> Json.Encode.Value
encodeScore x =
    Json.Encode.object
        [ ( "textLength", Json.Encode.int x.textLength )
        , ( "clearTime", Json.Encode.int x.clearTime )
        , ( "swapCount", Json.Encode.int x.swapCount )
        ]

getScores : Http.Request (List (Score))
getScores =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "scores"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeScore)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postScores : Score -> Http.Request (Score)
postScores body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "scores"
                ]
        , body =
            Http.jsonBody (encodeScore body)
        , expect =
            Http.expectJson decodeScore
        , timeout =
            Nothing
        , withCredentials =
            False
        }