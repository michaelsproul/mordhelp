module Main exposing (..)

import Attack exposing (toHitByWs, toWoundByStrength)
import Browser exposing (Document)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , option
        , p
        , select
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, selected)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import List exposing (map, range)
import Task exposing (Task)
import Warband exposing (Equipment(..), Unit, Warband, WeaponStrength(..), decodeWarband)



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
    , enemyWarband : Maybe Warband
    , warbands : List Warband
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = WarbandInfo, warband = Nothing, enemyWarband = Nothing, warbands = [] }, Cmd.none )



-- UPDATE


type Msg
    = WarbandsRequested
    | WarbandFilesSelected File (List File)
    | WarbandLoaded Warband
    | WarbandSelected String
    | EnemyWarbandSelected String
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
        WarbandsRequested ->
            ( model, Select.files [ "application/json" ] WarbandFilesSelected )

        WarbandFilesSelected file files ->
            let
                processFile f =
                    File.toString f |> Task.andThen (runDecoder decodeWarband)

                fileTasks =
                    List.map processFile (file :: files)

                fileCommands =
                    List.map (Task.attempt (handleError WarbandLoaded)) fileTasks
            in
            ( model, Cmd.batch fileCommands )

        WarbandLoaded warband ->
            let
                selectedWarband =
                    if List.isEmpty model.warbands then
                        Just warband

                    else
                        model.warband

                enemyWarband =
                    if List.length model.warbands == 1 then
                        Just warband

                    else
                        model.enemyWarband
            in
            ( { model
                | warband = selectedWarband
                , enemyWarband = enemyWarband
                , warbands = warband :: model.warbands
              }
            , Cmd.none
            )

        WarbandSelected name ->
            let
                warband =
                    List.head (List.filter (\w -> w.name == name) model.warbands)
            in
            ( { model | warband = warband }, Cmd.none )

        EnemyWarbandSelected name ->
            let
                warband =
                    List.head (List.filter (\w -> w.name == name) model.warbands)
            in
            ( { model | enemyWarband = warband }, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model.page of
        GenericStats ->
            { title = "Rules - Mordhelp", body = [ viewGenericStats ] }

        WarbandInfo ->
            { title = "Warband - Mordhelp", body = [ viewWarband model ] }


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

        rowHeader row =
            td [ class "row-header" ] [ text (String.fromInt row) ]

        buildRow row =
            tr [] (rowHeader row :: table2dRow width row f)
    in
    table []
        [ thead [] [ tr [] (cornerHeader :: columnHeaders) ]
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


diceRoll : Maybe Int -> String
diceRoll maybeN =
    maybeN
        |> Maybe.map (\n -> String.fromInt n ++ "+")
        |> Maybe.withDefault "-"


viewUnitMatchup : Unit -> List ( ( Int, Int ), List Unit ) -> Html Msg
viewUnitMatchup unit enemyUnits =
    let
        headers =
            [ unit.profile.name
            , "To hit"
            , "To wound"
            , "Rend"
            ]
                |> List.map (text >> List.singleton >> th [])

        matchName equipment ( _, enemies ) =
            td [] [ text <| equipment.name ++ " vs " ++ String.join ", " (List.map (.profile >> .name) enemies) ]

        toHit equipment ( ( enemyWs, _ ), _ ) =
            td [] [ text <| diceRoll <| Just <| toHitByWs unit.profile.weaponSkill enemyWs ]

        effectiveStrength weapon =
            case weapon.strength of
                StrengthConst n ->
                    n

                StrengthMod n ->
                    unit.profile.strength + n

        toWound weapon ( ( _, toughness ), _ ) =
            td [] [ text <| diceRoll <| toWoundByStrength (effectiveStrength weapon) toughness ]

        buildRow equipment enemy =
            tr []
                [ matchName equipment enemy
                , toHit equipment enemy
                , toWound equipment enemy
                ]

        -- TODO: filter melee weapons only
        weapons =
            List.map (\(EquipmentWeapon w) -> w) unit.equipment

        rows =
            List.concatMap (\equipment -> List.map (buildRow equipment) enemyUnits) weapons
    in
    table [ class "unit-matchup" ]
        [ thead [] headers
        , tbody [] rows
        ]



-- Group by (ws, toughness)


groupEnemyUnits : List Unit -> Dict ( Int, Int ) (List Unit)
groupEnemyUnits units =
    List.foldl
        (\unit ->
            Dict.update
                ( unit.profile.weaponSkill, unit.profile.toughness )
                (\existing -> Just (unit :: Maybe.withDefault [] existing))
        )
        Dict.empty
        units



-- TODO: ordered map?


viewUnitMatchups : Unit -> Warband -> Html Msg
viewUnitMatchups unit enemyWarband =
    let
        groups =
            Dict.toList (groupEnemyUnits enemyWarband.units)
    in
    viewUnitMatchup unit groups


viewAllUnitMatchups : Model -> List (Html Msg)
viewAllUnitMatchups model =
    case ( model.warband, model.enemyWarband ) of
        ( Just w, Just ew ) ->
            List.map (\unit -> viewUnitMatchups unit ew) w.units

        ( _, _ ) ->
            [ p [] [ text "Select a warband & enemy warband above" ] ]


selectOptions : Maybe Warband -> List Warband -> List (Html Msg)
selectOptions selectedWarband warbands =
    List.map (\w -> option [ selected (Just w == selectedWarband) ] [ text w.name ]) warbands


viewWarband : Model -> Html Msg
viewWarband model =
    div [] <|
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , button [ onClick WarbandsRequested ] [ text "Upload Warband(s)" ]
        , p [] [ text "Select your warband: " ]
        , select [ onInput WarbandSelected ] (selectOptions model.warband model.warbands)
        , p [] [ text "Select enemy warband: " ]
        , select [ onInput EnemyWarbandSelected ] (selectOptions model.enemyWarband model.warbands)
        , p [] [ text (Maybe.withDefault "[[ Select your warband ]]" (Maybe.map .name model.warband)) ]
        ]
            ++ viewAllUnitMatchups model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
