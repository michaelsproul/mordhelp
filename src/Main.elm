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
        , h4
        , h5
        , input
        , option
        , p
        , select
        , span
        , table
        , tbody
        , td
        , text
        , textarea
        , th
        , thead
        , tr
        )
import Html.Attributes as Attributes exposing (class, colspan, href, readonly, selected)
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
        , ModifierKind(..)
        , Profile
        , Unit
        , UnitKind(..)
        , Warband
        , WeaponKind(..)
        , WeaponStrength
        , decodeModifierKindStr
        , decodeUnitKindStr
        , decodeWarband
        , decodeWeaponKindStr
        , defaultUnit
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
      --| EditUnit is parameterised by the unit index to edit and an editing function.
    | EditUnit Int (Unit -> Unit)
      --| EditEquipment is parameterised by unit index and equipment index.
    | EditEquipment ( Int, Int ) (Equipment -> Equipment)
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

        EditEquipment ( unitIdx, equipmentIdx ) f ->
            let
                newWarband =
                    Maybe.map
                        (\warband ->
                            let
                                units =
                                    List.indexedMap
                                        (\i unit ->
                                            if i == unitIdx then
                                                let
                                                    newEquipment =
                                                        List.indexedMap
                                                            (\j equipment ->
                                                                if j == equipmentIdx then
                                                                    f equipment

                                                                else
                                                                    equipment
                                                            )
                                                            unit.equipment
                                                in
                                                { unit | equipment = newEquipment }

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
                Modifier Abs n ->
                    n

                Modifier Mod n ->
                    unit.profile.strength + n

        toWound weapon toughness =
            td [] [ text <| diceRoll <| toWoundByStrength (effectiveStrength weapon) toughness ]

        effectiveRend weapon =
            let
                rend =
                    rendByStrength (effectiveStrength weapon)
            in
            case weapon.rend of
                Modifier Abs n ->
                    n

                Modifier Mod n ->
                    rend + n

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


warbandSelectOptions : Maybe Warband -> List Warband -> List (Html Msg)
warbandSelectOptions selectedWarband warbands =
    List.map (\w -> option [ selected (Just w == selectedWarband) ] [ text w.name ]) warbands


unitKindSelectOptions : UnitKind -> List (Html Msg)
unitKindSelectOptions kind =
    [ option [ selected (kind == Hero) ] [ text "hero" ]
    , option [ selected (kind == Henchman) ] [ text "henchman" ]
    ]


weaponKindSelectOptions : WeaponKind -> List (Html Msg)
weaponKindSelectOptions kind =
    [ option [ selected (kind == Melee) ] [ text "melee" ]
    , option [ selected (kind == Ballistic) ] [ text "ballistic" ]
    ]


modifierKindSelectOptions : ModifierKind -> List (Html Msg)
modifierKindSelectOptions mod =
    [ option [ selected (mod == Mod) ] [ text "mod" ]
    , option [ selected (mod == Abs) ] [ text "abs" ]
    ]


superGenericEdit parser event accessor string =
    case parser string of
        Just value ->
            event (set accessor value)

        Nothing ->
            Debug.log ("invalid input: " ++ string) Noop


genericIntEdit event accessor string =
    case String.toInt string of
        Just value ->
            event (set accessor value)

        Nothing ->
            Debug.log "invalid input, not an integer" Noop


genericIntInput event ro initialValue accessor =
    input
        [ Attributes.value (String.fromInt initialValue)
        , readonly ro
        , onInput (genericIntEdit event accessor)
        ]
        []


genericStringEdit event accessor value =
    event (set accessor value)


genericStringInput event initialValue accessor =
    input
        [ Attributes.value initialValue
        , onInput (genericStringEdit event accessor)
        ]
        []



-- Editors and inputs


intEdit idx =
    genericIntEdit (EditUnit idx)


intInputRo ro initialValue idx accessor =
    genericIntInput (EditUnit idx) ro initialValue accessor


intInput =
    intInputRo False


stringEdit idx accessor value =
    genericStringEdit (EditUnit idx)


stringInput initialValue idx accessor =
    genericStringInput (EditUnit idx) initialValue accessor


unitKindSelect idx initialValue accessor =
    select [ onInput <| superGenericEdit decodeUnitKindStr (EditUnit idx) accessor ]
        (unitKindSelectOptions initialValue)


warbandIntEdit =
    genericIntEdit EditWarband


warbandIntInput initialValue accessor =
    genericIntInput EditWarband False initialValue accessor


warbandStringEdit =
    genericStringEdit EditWarband


warbandStringInput initialValue accessor =
    genericStringInput EditWarband initialValue accessor


equipmentIntEdit idx =
    genericIntEdit (EditEquipment idx)


equipmentIntInput idx =
    genericIntInput (EditEquipment idx) False


equipmentStringEdit idx =
    genericStringEdit (EditEquipment idx)


equipmentStringInput idx =
    genericStringInput (EditEquipment idx)


weaponKindSelect idx initialValue accessor =
    select [ onInput <| superGenericEdit decodeWeaponKindStr (EditEquipment idx) accessor ]
        (weaponKindSelectOptions initialValue)


modifierKindSelect idx equipment accessor =
    select [ onInput <| superGenericEdit decodeModifierKindStr (EditEquipment idx) accessor ]
        (modifierKindSelectOptions (get accessor equipment))


viewProfileStatBlock : Int -> Profile -> List (Html Msg)
viewProfileStatBlock idx profile =
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


viewAndEditEquipment : ( Int, Int ) -> Equipment -> List (Html Msg)
viewAndEditEquipment idx equipment =
    let
        (EquipmentWeapon weapon) =
            equipment
    in
    [ tr [] [ td [] [ equipmentStringInput idx weapon.name (equipmentWeapon << weaponName) ] ]
    , tr [] [ td [] [ weaponKindSelect idx weapon.kind (equipmentWeapon << weaponKind) ] ]
    , tr []
        [ td [] [ text "Strength: " ]
        , td [] [ modifierKindSelect idx equipment (equipmentWeapon << weaponStrength << modifierKind) ]
        , td [] [ equipmentIntInput idx (get modifierValue weapon.strength) (equipmentWeapon << weaponStrength << modifierValue) ]
        ]
    , tr []
        [ td [] [ text "Rend: " ]
        , td [] [ modifierKindSelect idx equipment (equipmentWeapon << weaponRend << modifierKind) ]
        , td [] [ equipmentIntInput idx (get modifierValue weapon.rend) (equipmentWeapon << weaponRend << modifierValue) ]
        ]
    ]


viewUnitEquipment : Int -> List Equipment -> List (Html Msg)
viewUnitEquipment unitIdx equipment =
    List.indexedMap (\equipIdx -> viewAndEditEquipment ( unitIdx, equipIdx )) equipment
        |> List.concat


headerCell : String -> Html Msg -> Html Msg
headerCell label content =
    div [] [ text (label ++ ": "), content ]


addEquipmentButton : Int -> Html Msg
addEquipmentButton idx =
    let
        newEquipment =
            EquipmentWeapon defaultWeapon

        addEquipmentMsg =
            EditUnit idx (\unit -> { unit | equipment = unit.equipment ++ [ newEquipment ] })
    in
    button [ onClick addEquipmentMsg ] [ text "Add Equipment" ]


viewAndEditUnit : Int -> Unit -> Html Msg
viewAndEditUnit idx unit =
    let
        width =
            9

        headerRow =
            tr [] [ th [ colspan width ] [ h4 [] [ text unit.name ] ] ]

        nameRow =
            tr [] [ td [ colspan width ] [ headerCell "Name" (stringInput unit.name idx unitName) ] ]

        kindRow =
            tr []
                [ td [ colspan width ]
                    [ headerCell "Kind" (unitKindSelect idx unit.profile.kind (unitProfile << profileKind)) ]
                ]

        countRow =
            tr []
                [ td [ colspan width ]
                    [ headerCell "Models"
                        (intInputRo (unit.profile.kind == Hero) unit.count idx unitCount)
                    ]
                ]

        xpRow =
            tr [] [ td [ colspan width ] [ headerCell "Experience (XP)" (intInput unit.xp idx unitXp) ] ]

        profileRows =
            viewProfileStatBlock idx unit.profile

        equipmentRows =
            [ tr [] [ td [ colspan width ] [ h5 [] [ text <| unit.name ++ "'s Equipment" ] ] ]
            , tr [] [ td [ colspan 2 ] [ addEquipmentButton idx ] ]
            ]
                ++ viewUnitEquipment idx unit.equipment

        rows =
            [ headerRow, nameRow, kindRow, countRow, xpRow ] ++ profileRows ++ equipmentRows
    in
    table [ class "unit" ]
        [ tbody [] rows ]


viewAndEditWarbandNotes : String -> List (Html Msg)
viewAndEditWarbandNotes notes =
    [ h2 [] [ text "Warband Notes" ]
    , textarea
        [ onInput (warbandStringEdit warbandNotes) ]
        [ text notes ]
    ]


addUnitButton : Html Msg
addUnitButton =
    let
        addUnitMsg =
            EditWarband (\warband -> { warband | units = warband.units ++ [ defaultUnit ] })
    in
    button [ onClick addUnitMsg ] [ text "Add Unit" ]


viewAndEditWarband : Warband -> List (Html Msg)
viewAndEditWarband warband =
    [ h2 [] [ headerCell "Name" (warbandStringInput warband.name warbandName) ]
    , p [] [ headerCell "Gold" (warbandIntInput warband.treasury.gold (warbandTreasury << treasuryGold)) ]
    , p [] [ headerCell "Wyrdstone" (warbandIntInput warband.treasury.wyrdstone (warbandTreasury << treasuryWyrdstone)) ]
    , h3 [] [ text "Units" ]
    , addUnitButton
    ]
        ++ List.indexedMap viewAndEditUnit warband.units
        ++ viewAndEditWarbandNotes warband.notes


viewWarband : Model -> Html Msg
viewWarband model =
    div [] <|
        [ h1 [] [ text "Mordhelp" ]
        , siteNav
        , button [ onClick WarbandsRequested ] [ text "Upload Warband(s)" ]
        , p [] [ text "Select your warband: " ]
        , select [ onInput WarbandSelected ] (warbandSelectOptions model.warband model.warbands)
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
        , select [ onInput WarbandSelected ] (warbandSelectOptions model.warband model.warbands)
        , p [] [ text "Select enemy warband: " ]
        , select [ onInput EnemyWarbandSelected ] (warbandSelectOptions model.enemyWarband model.warbands)
        ]
            ++ [ viewAllUnitMatchups model ]
            ++ [ viewFooter ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
