module Main exposing (main)

import AnaQRam.Generated.API as API
import AnaQRam.Puzzle as Puzzle exposing (Piece, Puzzle)
import AnaQRam.QRCode as QRCode exposing (QRCode)
import Browser as Browser
import Dict
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, autoplay, class, height, hidden, id, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Error, errorToString)
import Time


main : Program QRCode.Config Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { config : QRCode.Config
    , qrcode : Maybe QRCode
    , error : String
    , sizes : List Int
    , puzzle : Puzzle
    , click : Maybe Int
    , timer : Int
    , clear : Bool
    , swap : Int
    }


init : QRCode.Config -> ( Model, Cmd Msg )
init config =
    ( { config = config
      , qrcode = Nothing
      , error = ""
      , sizes = []
      , puzzle = Puzzle.empty
      , click = Nothing
      , timer = 0
      , clear = False
      , swap = 0
      }
    , API.getApiSizes FetchWordSizes
    )


type Msg
    = StartGame
    | FetchWordSizes (Result Http.Error (List Int))
    | FetchAnswer (Result Http.Error String)
    | ShufflePuzzle Puzzle
    | CaptureImage
    | UpdateQRCode (Result Error (Maybe QRCode))
    | ChoiceWordSize Int
    | ClickPiece Int
    | Tick Time.Posix
    | PostScore (Result Http.Error API.Score)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.puzzle.start, msg ) of
        ( False, StartGame ) ->
            ( { model
                | puzzle = Puzzle.start model.puzzle
                , timer = 0
                , clear = False
              }
            , Cmd.batch
                [ QRCode.startCamera ()
                , API.getApiProblem (Puzzle.size model.puzzle) FetchAnswer
                ]
            )

        ( False, FetchWordSizes (Ok sizes) ) ->
            ( { model | sizes = sizes }, Cmd.none )

        ( False, FetchWordSizes (Err err) ) ->
            ( { model | error = "can't fetch problem sizes: " ++ httpErrorToString err }, Cmd.none )

        ( True, FetchAnswer (Ok "") ) ->
            ( { model | error = "problem not found." }, Cmd.none )

        ( True, FetchAnswer (Ok answer) ) ->
            ( model, Puzzle.shuffle ShufflePuzzle (Puzzle.init answer model.puzzle) )

        ( True, FetchAnswer (Err err) ) ->
            ( { model | error = "can't fetch problem: " ++ httpErrorToString err }, Cmd.none )

        ( True, ShufflePuzzle puzzle ) ->
            ( { model | puzzle = puzzle }, Cmd.none )

        ( True, CaptureImage ) ->
            ( model, QRCode.captureImage () )

        ( True, UpdateQRCode (Ok Nothing) ) ->
            ( { model | error = "QR code is not found." }, Cmd.none )

        ( True, UpdateQRCode (Ok (Just qrcode)) ) ->
            updatePuzzle qrcode model

        ( _, UpdateQRCode (Err message) ) ->
            ( { model | error = errorToString message }, Cmd.none )

        ( _, ChoiceWordSize 0 ) ->
            ( model, Cmd.none )

        ( False, ChoiceWordSize wordSize ) ->
            ( { model | puzzle = Puzzle.dummy wordSize }, Cmd.none )

        ( True, ClickPiece idx ) ->
            updatePiece idx model

        ( _, Tick _ ) ->
            ( { model | timer = model.timer + 1 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updatePuzzle : QRCode -> Model -> ( Model, Cmd Msg )
updatePuzzle qrcode model =
    case String.toInt qrcode.data of
        Nothing ->
            ( { model | qrcode = Just qrcode, error = "" }, Cmd.none )

        Just pIdx ->
            let
                updated =
                    Puzzle.display pIdx model.puzzle
            in
            updateWithClear { model | qrcode = Just qrcode, error = "", puzzle = updated }


updatePiece : Int -> Model -> ( Model, Cmd Msg )
updatePiece idx model =
    case model.click of
        Nothing ->
            ( { model | click = Just idx }, Cmd.none )

        Just oldIdx ->
            let
                updated =
                    Puzzle.swapPiece idx oldIdx model.puzzle
            in
            updateWithClear { model | click = Nothing, puzzle = updated, swap = model.swap + 1 }


updateWithClear : Model -> ( Model, Cmd Msg )
updateWithClear model =
    if model.puzzle.start && Puzzle.success model.puzzle then
        ( { model | clear = True }, postScore model )

    else
        ( model, Cmd.none )


postScore : Model -> Cmd Msg
postScore model =
    let
        score =
            { textLength = Puzzle.size model.puzzle
            , clearTime = model.timer
            , swapCount = model.swap
            }
    in
    API.postApiScores score PostScore


view : Model -> Html Msg
view model =
    div []
        [ viewTimer model
        , video
            [ class "my-2"
            , id model.config.ids.video
            , style "background-color" "#000"
            , autoplay True
            , attribute "playsinline" ""
            , width model.config.size.width
            , height model.config.size.height
            ]
            []
        , p []
            [ viewSelectMenu model
            , button
                [ class "btn mx-1", type_ "button", onClick StartGame ]
                [ text "Game Start" ]
            , button
                [ class "btn mx-1", type_ "button", onClick CaptureImage ]
                [ text "Decode QR" ]
            ]
        , canvas [ id model.config.ids.capture, hidden True ] []
        , viewPuzzle model
        , viewResult model
        ]


viewTimer : Model -> Html msg
viewTimer model =
    div [ class "h2 my-1" ]
        [ String.concat
            [ String.padLeft 2 '0' <| String.fromInt (model.timer // 60)
            , ":"
            , String.padLeft 2 '0' <| String.fromInt (modBy 60 model.timer)
            ]
            |> text
        ]


viewSelectMenu : Model -> Html Msg
viewSelectMenu model =
    let
        viewItem v =
            option [ value (String.fromInt v) ] [ text (String.fromInt v) ]
    in
    select
        [ class "form-select mx-1"
        , onInput (ChoiceWordSize << Maybe.withDefault 0 << String.toInt)
        ]
        (option [] [ text "Choose Word Size" ] :: List.map viewItem model.sizes)


viewResult : Model -> Html Msg
viewResult model =
    let
        scaned =
            model.qrcode
                |> Maybe.map .data
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\idx -> Puzzle.getPiece idx model.puzzle)

        attr =
            class "mx-5 mb-2 text-left"
    in
    case ( Puzzle.success model.puzzle, model.error, scaned ) of
        ( True, _, _ ) ->
            div [ attr, class "flash" ] [ text "Success!!" ]

        ( _, "", Just piece ) ->
            div [ attr, class "flash flash-success" ] [ text ("Found Piece: " ++ String.fromChar piece.char) ]

        ( _, "", Nothing ) ->
            div [] [ text "" ]

        _ ->
            div [ attr, class "flash flash-error" ] [ text ("Error: " ++ model.error) ]


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div [ class "mb-2" ] (Puzzle.map (viewPiece model) model.puzzle)


viewPiece : Model -> Int -> Piece -> Html Msg
viewPiece model viewIdx piece =
    let
        clicked =
            if Just viewIdx == model.click then
                class "btn btn-danger"

            else
                class "btn"
    in
    button [ class "mx-1", type_ "button", clicked, onClick (ClickPiece viewIdx) ]
        [ text (Puzzle.pieceToString piece) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ QRCode.updateQRCodeWithDecode UpdateQRCode
        , if model.puzzle.start && not model.clear then
            Time.every 1000 Tick

          else
            Sub.none
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl str ->
            "bad url: " ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody str ->
            "bad body: " ++ str
