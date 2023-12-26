module Main exposing (..)

import Attack exposing (toHitByWs, toWoundByStrength)
import Browser exposing (Document)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h1, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import List exposing (map, range)
import Task exposing (Task)
import Warband exposing (Warband, decodeWarband)



-- MAIN


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Page
    = GenericStats
    | WarbandInfo


type alias Model =
    { page : Page
    , warband : Maybe Warband
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = WarbandInfo, warband = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = WarbandRequested
    | WarbandSelected File
    | WarbandLoaded Warband
    | ChangePage Page
    | Noop


resultToTask : Result x a -> Task x a
resultToTask res =
    case res of
        Ok v ->
            Task.succeed v

        Err e ->
            Task.fail e


runDecoder : Decoder a -> String -> Task Decode.Error a
runDecoder decoder value =
    Decode.decodeString decoder value |> resultToTask


handleError : (a -> Msg) -> Result x a -> Msg
handleError f res =
    case res of
        Ok v ->
            f v

        Err e ->
            Debug.log ("failed: " ++ Debug.toString e) Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WarbandRequested ->
            ( model, Select.file [ "application/json" ] WarbandSelected )

        WarbandSelected file ->
            ( model, Task.attempt (handleError WarbandLoaded) (File.toString file |> Task.andThen (runDecoder decodeWarband)) )

        WarbandLoaded warband ->
            ( { page = model.page, warband = Just warband }, Cmd.none )

        ChangePage page ->
            ( { page = page, warband = model.warband }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model.page of
        GenericStats ->
            { title = "Rules - Mordhelp", body = [ viewGenericStats ] }

        WarbandInfo ->
            { title = "Warband - Mordhelp", body = [ viewWarband model.warband ] }


table2dRow : Int -> Int -> (Int -> Int -> String) -> List (Html Msg)
table2dRow width row f =
    range 1 width |> map (\col -> td [] [ text (f row col) ])


table2d : Int -> Int -> (Int -> Int -> String) -> Html Msg
table2d width height f =
    let
        cornerHeader =
            th [] [ text " " ]

        columnHeaders =
            range 1 width |> map (\i -> th [] [ text (String.fromInt i) ])

        rowHeader =
            \row -> td [ class "row-header" ] [ text (String.fromInt row) ]

        buildRow =
            \row -> tr [] ([ rowHeader row ] ++ table2dRow width row f)
    in
    table []
        [ thead [] [ tr [] ([ cornerHeader ] ++ columnHeaders) ]
        , tbody [] (range 1 height |> map buildRow)
        ]


siteNav : Html Msg
siteNav =
    div []
        [ span [ onClick (ChangePage GenericStats) ] [ text "Stats" ]
        , span [ onClick (ChangePage WarbandInfo) ] [ text "Warband" ]
        ]


viewGenericStats : Html Msg
viewGenericStats =
    div []
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , table2d 10 10 (\attackerWs defenderWs -> String.fromInt (toHitByWs attackerWs defenderWs))
        , table2d 10
            10
            (\strength toughness ->
                toWoundByStrength strength toughness
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault "-"
            )
        ]


viewWarband : Maybe Warband -> Html Msg
viewWarband maybeWarband =
    div []
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , button [ onClick WarbandRequested ] [ text "Upload Warband" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
