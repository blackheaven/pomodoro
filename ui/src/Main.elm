module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D


serverUrl : String
serverUrl =
    "http://localhost:8080/"


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = always Sub.none, view = view }


type Model
    = Nope


type Msg
    = AskStatus
    | GotStatus (Result Http.Error ())


init : () -> ( Model, Cmd Msg )
init () =
    ( Nope, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AskStatus ->
            ( model
            , Http.get
                { url = String.append serverUrl "status"
                , expect = Http.expectJson GotStatus decodeStatus
                }
            )

        GotStatus _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AskStatus ] [ text "Status" ]
        ]


decodeStatus : D.Decoder ()
decodeStatus =
    D.succeed ()
