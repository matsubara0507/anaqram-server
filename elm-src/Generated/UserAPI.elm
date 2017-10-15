module Generated.UserAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { name : String
    , textLength : Int
    , clearTime : Int
    , swapCount : Int
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string
        |> required "textLength" int
        |> required "clearTime" int
        |> required "swapCount" int

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "textLength", Json.Encode.int x.textLength )
        , ( "clearTime", Json.Encode.int x.clearTime )
        , ( "swapCount", Json.Encode.int x.swapCount )
        ]

getUsers : Http.Request (List (User))
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postUsers : User -> Http.Request (User)
postUsers body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "users"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }