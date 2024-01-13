module Main exposing (..)

import Accessors exposing (get, set)
import Attack exposing (rendByStrength, toHitBallistic, toHitByWs, toWoundByStrength)
import Browser exposing (Document)
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select as Select
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , h2
        , h3
        , input
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
import Html.Attributes as Attributes exposing (class, colspan, href, selected)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Lenses exposing (..)
import List exposing (map, range)
import Set exposing (Set)
import Task exposing (Task)
import Warband
    exposing
        ( Equipment(..)
        , Modifier(..)
        , Profile
        , Unit
        , Warband
        , WeaponKind(..)
        , WeaponStrength
        , decodeWarband
        , defaultWeapon
        , encodeWarband
        )



-- MAIN


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Page
    = GenericRules
    | WarbandInfo
    | Matchups


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
    | DownloadWarband
    | ChangePage Page
      --| EditUnit is parameterised by the unit index to edit and an editing function
    | EditUnit Int (Unit -> Unit)
    | EditWarband (Warband -> Warband)
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

        DownloadWarband ->
            case model.warband of
                Just w ->
                    let
                        indent =
                            2

                        jsonString =
                            Json.Encode.encode indent (encodeWarband w)
                    in
                    ( model, File.Download.string "warband.json" "application/json" jsonString )

                Nothing ->
                    ( model, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        EditUnit idx f ->
            let
                newWarband =
                    Maybe.map
                        (\warband ->
                            let
                                units =
                                    List.indexedMap
                                        (\i unit ->
                                            if i == idx then
                                                f unit

                                            else
                                                unit
                                        )
                                        warband.units
                            in
                            { warband | units = units }
                        )
                        model.warband
            in
            ( { model | warband = newWarband }, Cmd.none )

        EditWarband f ->
            let
                newWarband =
                    Maybe.map f model.warband
            in
            ( { model | warband = newWarband }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model.page of
        GenericRules ->
            { title = "Rules - Mordhelp", body = [ viewGenericRules ] }

        WarbandInfo ->
            { title = "Warband - Mordhelp", body = [ viewWarband model ] }

        Matchups ->
            { title = "Warband - Mordhelp", body = [ viewMatchups model ] }


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
    div [ class "nav" ]
        [ span [ onClick (ChangePage GenericRules) ] [ text "Rules" ]
        , span [ onClick (ChangePage WarbandInfo) ] [ text "Warband" ]
        , span [ onClick (ChangePage Matchups) ] [ text "Matchups" ]
        ]


viewGenericRules : Html Msg
viewGenericRules =
    div []
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , h2 [] [ text "To hit" ]
        , table2d 10 10 (\attackerWs defenderWs -> String.fromInt (toHitByWs attackerWs defenderWs))
        , h2 [] [ text "To wound" ]
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


renderRend : Int -> String
renderRend n =
    if n == 0 then
        "-"

    else
        String.fromInt n


viewUnitMatchup :
    Unit
    -> List ( ( Int, Int ), Set String )
    -> List ( Int, Set String )
    -> List (Html Msg)
viewUnitMatchup unit enemyUnitsByWsToughness enemyUnitsByToughness =
    let
        headers =
            [ unit.profile.name
            , "To hit"
            , "To wound"
            , "Rend"
            ]
                |> List.map (text >> List.singleton >> th [])

        matchName equipment ( _, enemies ) =
            td [] [ text <| equipment.name ++ " vs " ++ String.join ", " (Set.toList enemies) ]

        toHit equipment ( ( enemyWs, _ ), _ ) =
            td [] [ text <| diceRoll <| Just <| toHitByWs unit.profile.weaponSkill enemyWs ]

        toHitBallisticCell =
            td [] [ text <| diceRoll <| Just <| toHitBallistic unit.profile.ballisticSkill ]

        effectiveStrength weapon =
            case weapon.strength of
                ModifierAbs n ->
                    n

                ModifierMod n ->
                    unit.profile.strength + n

        toWound weapon toughness =
            td [] [ text <| diceRoll <| toWoundByStrength (effectiveStrength weapon) toughness ]

        effectiveRend weapon =
            let
                rend =
                    rendByStrength (effectiveStrength weapon)
            in
            case weapon.rend of
                Just (ModifierAbs n) ->
                    n

                Just (ModifierMod n) ->
                    rend + n

                Nothing ->
                    rend

        getRend weapon =
            td [] [ text <| renderRend <| effectiveRend weapon ]

        buildMeleeRow equipment enemy =
            tr []
                [ matchName equipment enemy
                , toHit equipment enemy
                , toWound equipment (Tuple.first enemy |> Tuple.second)
                , getRend equipment
                ]

        buildBallisticRow equipment enemy =
            tr []
                [ matchName equipment enemy
                , toHitBallisticCell
                , toWound equipment (Tuple.first enemy)
                , getRend equipment
                ]

        equippedWeapons =
            List.map (\(EquipmentWeapon w) -> w) unit.equipment

        ( equippedMeleeWeapons, ballisticWeapons ) =
            List.partition
                (\w ->
                    case w.kind of
                        Melee ->
                            True

                        Ballistic ->
                            False
                )
                equippedWeapons

        meleeWeapons =
            if List.isEmpty equippedMeleeWeapons then
                [ defaultWeapon ]

            else
                equippedMeleeWeapons

        meleeRows =
            List.concatMap (\equipment -> List.map (buildMeleeRow equipment) enemyUnitsByWsToughness) meleeWeapons

        ballisticRows =
            List.concatMap (\equipment -> List.map (buildBallisticRow equipment) enemyUnitsByToughness) ballisticWeapons

        rows =
            meleeRows ++ ballisticRows
    in
    [ thead [] headers
    , tbody [] rows
    ]



-- Group by (ws, toughness) for melee attacks and (toughness)


groupEnemyUnits : List Unit -> ( Dict ( Int, Int ) (Set String), Dict Int (Set String) )
groupEnemyUnits units =
    List.foldl
        (\unit ( byWsToughness, byToughness ) ->
            ( Dict.update
                ( unit.profile.weaponSkill, unit.profile.toughness )
                (\existing -> Just (Set.insert unit.profile.name (Maybe.withDefault Set.empty existing)))
                byWsToughness
            , Dict.update
                unit.profile.toughness
                (\existing -> Just (Set.insert unit.profile.name (Maybe.withDefault Set.empty existing)))
                byToughness
            )
        )
        ( Dict.empty, Dict.empty )
        units


viewUnitMatchups : Unit -> Warband -> List (Html Msg)
viewUnitMatchups unit enemyWarband =
    let
        ( meleeGroups, ballisticGroups ) =
            groupEnemyUnits enemyWarband.units

        sortedMeleeGroups =
            List.sortBy
                (\( ( ws, toughness ), unitNames ) -> ( ws, toughness, Set.size unitNames ))
                (Dict.toList meleeGroups)

        sortedBallisticGroups =
            List.sortBy (\( toughness, _ ) -> toughness) (Dict.toList ballisticGroups)
    in
    viewUnitMatchup unit sortedMeleeGroups sortedBallisticGroups


viewAllUnitMatchups : Model -> Html Msg
viewAllUnitMatchups model =
    case ( model.warband, model.enemyWarband ) of
        ( Just w, Just ew ) ->
            table [ class "unit-matchup" ] <|
                List.concatMap (\unit -> viewUnitMatchups unit ew) w.units

        ( _, _ ) ->
            p [] [ text "Upload warbands then select a warband & enemy warband above" ]


selectOptions : Maybe Warband -> List Warband -> List (Html Msg)
selectOptions selectedWarband warbands =
    List.map (\w -> option [ selected (Just w == selectedWarband) ] [ text w.name ]) warbands


intEdit idx accessor string =
    case String.toInt string of
        Just value ->
            EditUnit idx (\unit -> set accessor value unit)

        Nothing ->
            Debug.log "invalid input, not an integer" Noop


intInput initialValue idx accessor =
    input
        [ Attributes.value (String.fromInt initialValue)
        , onInput (\string -> intEdit idx accessor string)
        ]
        []


stringEdit idx accessor value =
    EditUnit idx (\unit -> set accessor value unit)


stringInput initialValue idx accessor =
    input
        [ Attributes.value initialValue
        , onInput (\string -> stringEdit idx accessor string)
        ]
        []


warbandIntEdit accessor string =
    case String.toInt string of
        Just value ->
            EditWarband (set accessor value)

        Nothing ->
            Debug.log "invalid input, not an integer" Noop


warbandIntInput initialValue accessor =
    input
        [ Attributes.value (String.fromInt initialValue)
        , onInput (warbandIntEdit accessor)
        ]
        []


warbandStringEdit accessor value =
    EditWarband (set accessor value)


warbandStringInput initialValue accessor =
    input
        [ Attributes.value initialValue
        , onInput (warbandStringEdit accessor)
        ]
        []


viewProfile : Int -> Profile -> List (Html Msg)
viewProfile idx profile =
    let
        header t =
            th [] [ text t ]

        cell initialValue accessor =
            td [] [ intInput initialValue idx (unitProfile << accessor) ]
    in
    [ tr []
        [ header "M"
        , header "WS"
        , header "BS"
        , header "S"
        , header "T"
        , header "W"
        , header "I"
        , header "A"
        , header "Ld"
        ]
    , tr []
        [ cell profile.movement profileMovement
        , cell profile.weaponSkill profileWeaponSkill
        , cell profile.ballisticSkill profileBallisticSkill
        , cell profile.strength profileStrength
        , cell profile.toughness profileToughness
        , cell profile.wounds profileWounds
        , cell profile.initiative profileInitiative
        , cell profile.attacks profileAttacks
        , cell profile.leadership profileLeadership
        ]
    ]


headerCell : String -> Html Msg -> Html Msg
headerCell label content =
    div [] [ text (label ++ ": "), content ]


viewAndEditUnit : Int -> Unit -> Html Msg
viewAndEditUnit idx unit =
    let
        width =
            9

        nameRow =
            tr [] [ td [ colspan width ] [ headerCell "Name" (stringInput unit.name idx unitName) ] ]

        countRow =
            tr []
                [ td [ colspan width ] [ headerCell "Models" (intInput unit.count idx unitCount) ] ]

        xpRow =
            tr [] [ td [ colspan width ] [ headerCell "Experience (XP)" (intInput unit.xp idx unitXp) ] ]

        profileRows =
            viewProfile idx unit.profile

        rows =
            [ nameRow, countRow, xpRow ] ++ profileRows
    in
    table [ class "unit" ]
        [ tbody [] rows ]


viewAndEditWarband : Warband -> List (Html Msg)
viewAndEditWarband warband =
    [ h2 [] [ headerCell "Name" (warbandStringInput warband.name warbandName) ]
    , p [] [ headerCell "Gold" (warbandIntInput warband.treasury.gold (warbandTreasury << treasuryGold)) ]
    , p [] [ headerCell "Wyrdstone" (warbandIntInput warband.treasury.wyrdstone (warbandTreasury << treasuryWyrdstone)) ]
    , h3 [] [ text "Units" ]
    ]
        ++ List.indexedMap viewAndEditUnit warband.units


viewWarband : Model -> Html Msg
viewWarband model =
    div [] <|
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , button [ onClick WarbandsRequested ] [ text "Upload Warband(s)" ]
        , p [] [ text "Select your warband: " ]
        , select [ onInput WarbandSelected ] (selectOptions model.warband model.warbands)
        ]
            ++ (case model.warband of
                    Just w ->
                        [ button [ onClick DownloadWarband ] [ text "Download Warband" ] ]
                            ++ viewAndEditWarband w

                    Nothing ->
                        []
               )
            ++ [ viewFooter ]


repoUrl : String
repoUrl =
    "https://github.com/michaelsproul/mordhelp"


viewFooter : Html Msg
viewFooter =
    p [ class "footer" ] [ text "Source available on ", a [ href repoUrl ] [ text "GitHub" ] ]


viewMatchups : Model -> Html Msg
viewMatchups model =
    div [] <|
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , button [ onClick WarbandsRequested ] [ text "Upload Warband(s)" ]
        , p [] [ text "Select your warband: " ]
        , select [ onInput WarbandSelected ] (selectOptions model.warband model.warbands)
        , p [] [ text "Select enemy warband: " ]
        , select [ onInput EnemyWarbandSelected ] (selectOptions model.enemyWarband model.warbands)
        ]
            ++ [ viewAllUnitMatchups model ]
            ++ [ viewFooter ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
