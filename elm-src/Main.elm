module Main exposing (..)

import Data.Composition exposing (..)
import Generated.ScoreAPI as API exposing (Score)
import Html as Html exposing (..)
import Html.Attributes exposing (checked, class, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import RemoteData exposing (RemoteData(..))
import Time exposing (Time, minute)


main : Program Never Model Msg
main =
    Html.program
        { init = init model
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { scores : RemoteData String (List API.Score)
    , reload : Bool
    , scoreOrds : List ScoreOrd
    }


type ScoreOrd
    = TextLength
    | ClearTime
    | SwapCount


type Msg
    = FetchScores (Result Http.Error (List API.Score))
    | CheckReload Bool
    | Reload
    | Tick Time
    | Click ScoreOrd


model : Model
model =
    { scores = NotAsked
    , reload = False
    , scoreOrds = [ TextLength, ClearTime, SwapCount ]
    }


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, fetchScores )


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "my-3 mx-auto col-10 col-lg-8" ]
            [ div []
                [ h2
                    [ class "f1-light float-left link-gray-dark"
                    , onClick Reload
                    ]
                    [ text "AnaQRam ランキング !!" ]
                , div [ class "float-right" ] [ viewCheckReload model ]
                ]
            , viewScores model
            ]
        ]


viewCheckReload : Model -> Html Msg
viewCheckReload model =
    form []
        [ div [ class "form-checkbox" ]
            [ label []
                [ input
                    [ type_ "checkbox"
                    , checked model.reload
                    , onCheck CheckReload
                    ]
                    []
                , text "Auto Reload"
                ]
            ]
        ]


viewScores : Model -> Html Msg
viewScores model =
    case model.scores of
        NotAsked ->
            text "Please Push Button."

        Loading ->
            text "Loading..."

        Failure err ->
            text ("Error: " ++ toString err)

        Success scores ->
            table
                [ class "col-12 f3" ]
                [ thead []
                    [ tr [ class "border-bottum" ]
                        [ th [ class "text-right p-2" ] [ text "順位" ]
                        , th [ class "text-right p-2 link-gray-dark"
                             , onClick $ Click TextLength
                             ]
                             [ text "文字数" ]
                        , th [ class "text-right p-2 link-gray-dark"
                             , onClick $ Click ClearTime
                             ]
                             [ text "クリアタイム" ]
                        , th [ class "text-right p-2 link-gray-dark"
                             , onClick $ Click SwapCount
                             ]
                             [ text "入れ替え回数" ]
                        ]
                    ]
                , scores
                    |> List.sortWith (compareScore model.scoreOrds)
                    |> List.indexedMap viewScore
                    |> tbody []
                ]


compareScore : List ScoreOrd -> Score -> Score -> Order
compareScore ords a b =
    List.foldl
        (\ord acc ->
            if acc /= EQ then
                acc
            else
                compareScoreWith ord a b
        )
        EQ
        ords


compareScoreWith : ScoreOrd -> Score -> Score -> Order
compareScoreWith ord a b =
    case ord of
        TextLength ->
            compare b.textLength a.textLength

        ClearTime ->
            compare a.clearTime b.clearTime

        SwapCount ->
            compare a.swapCount b.swapCount


viewScore : Int -> API.Score -> Html Msg
viewScore index score =
    tr
        [ class "border-top"
        , class
            $ (if index % 2 == 0 then
                "bg-gray-light"
               else
                ""
              )
        ]
        [ td [ class "text-right p-2" ] [ text $ toString (index + 1) ]
        , td [ class "text-right p-2" ] [ text $ toString score.textLength ]
        , td [ class "text-right p-2" ] [ text $ toString score.clearTime ]
        , td [ class "text-right p-2" ] [ text $ toString score.swapCount ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchScores (Ok scores) ->
            ( { model | scores = Success scores }, Cmd.none )

        FetchScores (Err _) ->
            ( { model | scores = Failure "Something went wrong.." }, Cmd.none )

        CheckReload reload ->
            ( { model | reload = reload }, Cmd.none )

        Reload ->
            ( model, fetchScores )

        Tick _ ->
            ( model
            , if model.reload then
                fetchScores
              else
                Cmd.none
            )

        Click ord ->
            let
                newScoreOrds =
                    ord :: List.filter ((/=) ord) model.scoreOrds
            in
            ( { model | scoreOrds = newScoreOrds }, Cmd.none )


fetchScores : Cmd Msg
fetchScores =
    Http.send FetchScores API.getScores


baseUrl : String
baseUrl =
    "localhost:8000"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every minute Tick
