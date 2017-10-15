module Main exposing (..)

import Data.Composition exposing (..)
import Generated.UserAPI as API exposing (User)
import Html as Html exposing (..)
import Html.Attributes exposing (class)
import Http
import RemoteData exposing (RemoteData(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init model
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { users : RemoteData String (List API.User)
    }


type Msg
    = FetchUsers (Result Http.Error (List API.User))


model : Model
model =
    { users = NotAsked }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, fetchUsers )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class " my-3 mx-auto col-10 col-lg-8" ]
            [ h2 [ class "f1-light" ] [ text "AnaQRam Ranking !!" ]
            , viewUsers model
            ]
        ]


viewUsers : Model -> Html Msg
viewUsers model =
    case model.users of
        NotAsked ->
            text "Please Push Button."

        Loading ->
            text "Loading..."

        Failure err ->
            text ("Error: " ++ toString err)

        Success users ->
            table
                [ class "col-12 f3" ]
                [ thead []
                    [ tr []
                        [ th [ class "text-left" ] [ text "Name" ]
                        , th [ class "text-right" ] [ text "Text Length" ]
                        , th [ class "text-right" ] [ text "Clear Time" ]
                        , th [ class "text-right" ] [ text "Swap Count" ]
                        ]
                    ]
                , users
                    |> List.sortBy .swapCount
                    |> List.sortBy .clearTime
                    |> List.sortBy .textLength
                    |> List.reverse
                    |> List.map viewUser
                    |> tbody []
                ]


viewUser : API.User -> Html Msg
viewUser user =
    tr []
        [ td [ class "text-left" ] [ text user.name ]
        , td [ class "text-right" ] [ text $ toString user.textLength ]
        , td [ class "text-right" ] [ text $ toString user.clearTime ]
        , td [ class "text-right" ] [ text $ toString user.swapCount ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchUsers (Ok users) ->
            ( { model | users = Success users }, Cmd.none )

        FetchUsers (Err _) ->
            ( { model | users = Failure "Something went wrong.." }, Cmd.none )


fetchUsers : Cmd Msg
fetchUsers =
    Http.send FetchUsers API.getUsers


baseUrl : String
baseUrl =
    "localhost:8000"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
