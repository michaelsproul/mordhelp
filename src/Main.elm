module Main exposing (..)

import Attack exposing (toHitByWs, toWoundByStrength)
import Browser exposing (Document)
import Html exposing (Html, div, h1, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (map, range)
import Warband



-- MAIN


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Page
    = GenericStats


type alias Model =
    { page : Page
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = GenericStats }, Cmd.none )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model.page of
        GenericStats ->
            { title = "Mordhelp: Rules", body = [ viewGenericStats ] }


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


viewGenericStats : Html Msg
viewGenericStats =
    div []
        [ h1 [] [ text "Mordhelp" ]
        , table2d 10 10 (\attackerWs defenderWs -> String.fromInt (toHitByWs attackerWs defenderWs))
        , table2d 10
            10
            (\strength toughness ->
                toWoundByStrength strength toughness
                    |> Maybe.map String.fromInt
                    |> Maybe.withDefault "-"
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
