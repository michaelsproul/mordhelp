module Warband exposing
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
    , defaultProfile
    , defaultUnit
    , defaultWeapon
    , encodeWarband
    )

import Json.Decode as Decode exposing (Decoder, int, list, maybe, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E


minCharacteristic : Int
minCharacteristic =
    1


maxCharacteristic : Int
maxCharacteristic =
    10


type alias Profile =
    { name : String
    , kind : UnitKind
    , movement : Int
    , weaponSkill : Int
    , ballisticSkill : Int
    , strength : Int
    , toughness : Int
    , wounds : Int
    , initiative : Int
    , attacks : Int
    , leadership : Int
    , specialRules : Maybe (List String)
    }


type UnitKind
    = Hero
    | Henchman


type WeaponKind
    = Melee
    | Ballistic


type ModifierKind
    = Mod
    | Abs


type Modifier
    = Modifier ModifierKind Int


type alias WeaponStrength =
    Modifier


type alias WeaponRend =
    Modifier


type alias Weapon =
    { name : String
    , kind : WeaponKind
    , strength : WeaponStrength
    , rend : WeaponRend
    , specialRules : Maybe (List String)
    }


type Equipment
    = EquipmentWeapon Weapon


type alias Unit =
    { count : Int
    , name : String
    , xp : Int
    , profile : Profile
    , equipment : List Equipment

    -- TODO
    -- , specialRules : Maybe (List String)
    }


type alias Treasury =
    { gold : Int
    , wyrdstone : Int
    , equipment : List Equipment
    }


type alias Warband =
    { name : String, treasury : Treasury, units : List Unit, notes : String }


defaultWeapon : Weapon
defaultWeapon =
    { name = "Unarmed strike"
    , kind = Melee
    , strength = Modifier Mod 0
    , rend = Modifier Mod 0
    , specialRules = Nothing
    }


defaultProfile : Profile
defaultProfile =
    { name = "<insert profile name>"
    , kind = Hero
    , movement = 0
    , weaponSkill = 0
    , ballisticSkill = 0
    , strength = 0
    , toughness = 0
    , wounds = 0
    , initiative = 0
    , attacks = 0
    , leadership = 0
    , specialRules = Nothing
    }


defaultUnit : Unit
defaultUnit =
    { count = 1
    , name = "<insert character name>"
    , xp = 0
    , profile = defaultProfile
    , equipment = []
    }


encodeMaybeField : String -> Maybe a -> (a -> E.Value) -> List ( String, E.Value )
encodeMaybeField name m enc =
    case m of
        Just v ->
            [ ( name, enc v ) ]

        Nothing ->
            []


decodeLiteral : String -> String -> Decoder String
decodeLiteral desired actual =
    if actual == desired then
        Decode.succeed desired

    else
        Decode.fail <| "expected `" ++ desired ++ "` got `" ++ actual


decodeLiteralAsValue : String -> a -> String -> Decoder a
decodeLiteralAsValue desired value actual =
    decodeLiteral desired actual |> Decode.map (\_ -> value)


decodeUnitKind : Decoder UnitKind
decodeUnitKind =
    Decode.oneOf
        [ string |> Decode.andThen (decodeLiteralAsValue "hero" Hero)
        , string |> Decode.andThen (decodeLiteralAsValue "henchman" Henchman)
        ]


decodeStr : Decoder a -> String -> Maybe a
decodeStr decoder s =
    case Decode.decodeString decoder ("\"" ++ s ++ "\"") of
        Ok kind ->
            Just kind

        Err _ ->
            Nothing


decodeUnitKindStr : String -> Maybe UnitKind
decodeUnitKindStr =
    decodeStr decodeUnitKind


encodeUnitKind : UnitKind -> E.Value
encodeUnitKind kind =
    case kind of
        Hero ->
            E.string "hero"

        Henchman ->
            E.string "henchman"


decodeWeaponKind : Decoder WeaponKind
decodeWeaponKind =
    Decode.oneOf
        [ string |> Decode.andThen (decodeLiteralAsValue "melee" Melee)
        , string |> Decode.andThen (decodeLiteralAsValue "ballistic" Ballistic)
        ]


decodeWeaponKindStr : String -> Maybe WeaponKind
decodeWeaponKindStr =
    decodeStr decodeWeaponKind


encodeWeaponKind : WeaponKind -> E.Value
encodeWeaponKind kind =
    case kind of
        Melee ->
            E.string "melee"

        Ballistic ->
            E.string "ballistic"


decodeProfile : Decoder Profile
decodeProfile =
    Decode.succeed Profile
        |> required "name" string
        |> required "kind" decodeUnitKind
        |> required "movement" int
        |> required "weaponSkill" int
        |> required "ballisticSkill" int
        |> required "strength" int
        |> required "toughness" int
        |> required "wounds" int
        |> required "initiative" int
        |> required "attacks" int
        |> required "leadership" int
        |> optional "specialRules" (maybe (list string)) Nothing


encodeProfile : Profile -> E.Value
encodeProfile profile =
    E.object <|
        [ ( "name", E.string profile.name )
        , ( "kind", encodeUnitKind profile.kind )
        , ( "movement", E.int profile.movement )
        , ( "weaponSkill", E.int profile.weaponSkill )
        , ( "ballisticSkill", E.int profile.ballisticSkill )
        , ( "strength", E.int profile.strength )
        , ( "toughness", E.int profile.toughness )
        , ( "wounds", E.int profile.wounds )
        , ( "initiative", E.int profile.initiative )
        , ( "attacks", E.int profile.attacks )
        , ( "leadership", E.int profile.leadership )
        ]
            ++ encodeMaybeField "specialRules" profile.specialRules (E.list E.string)


decodeModifierKind : Decoder ModifierKind
decodeModifierKind =
    Decode.oneOf
        [ string |> Decode.andThen (decodeLiteralAsValue "mod" Mod)
        , string |> Decode.andThen (decodeLiteralAsValue "abs" Abs)
        ]


decodeModifier : Decoder Modifier
decodeModifier =
    string
        |> Decode.andThen
            (\s ->
                let
                    ( s1, s2 ) =
                        ( String.left 3 s, String.dropLeft 3 s )
                in
                case ( decodeModifierKindStr s1, String.toInt s2 ) of
                    ( Just kind, Just n ) ->
                        Decode.succeed <| Modifier kind n

                    _ ->
                        Decode.fail <| "invalid modifier: " ++ s
            )


decodeModifierKindStr : String -> Maybe ModifierKind
decodeModifierKindStr =
    decodeStr decodeModifierKind


encodeModifier : Modifier -> E.Value
encodeModifier modifier =
    case modifier of
        Modifier Abs n ->
            E.string <| "abs" ++ String.fromInt n

        Modifier Mod n ->
            E.string <| "mod" ++ String.fromInt n


decodeEquipment : Decoder Equipment
decodeEquipment =
    Decode.succeed
        (\name kind strength rend specialRules ->
            EquipmentWeapon
                { name = name
                , kind = kind
                , strength = strength
                , rend = rend
                , specialRules = specialRules
                }
        )
        |> required "name" string
        |> required "kind" decodeWeaponKind
        |> required "strength" decodeModifier
        |> optional "rend" decodeModifier (Modifier Mod 0)
        |> optional "specialRules" (maybe (list string)) Nothing


encodeEquipment : Equipment -> E.Value
encodeEquipment equipment =
    case equipment of
        EquipmentWeapon w ->
            encodeWeapon w


encodeWeapon : Weapon -> E.Value
encodeWeapon weapon =
    let
        maybeRend =
            if weapon.rend == Modifier Mod 0 then
                Nothing

            else
                Just weapon.rend
    in
    E.object <|
        [ ( "name", E.string weapon.name )
        , ( "kind", encodeWeaponKind weapon.kind )
        , ( "strength", encodeModifier weapon.strength )
        ]
            ++ encodeMaybeField "rend" maybeRend encodeModifier
            ++ encodeMaybeField "specialRules" weapon.specialRules (E.list E.string)


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed Unit
        |> required "count" int
        |> required "name" string
        |> required "xp" int
        |> required "profile" decodeProfile
        |> required "equipment" (list decodeEquipment)


encodeUnit : Unit -> E.Value
encodeUnit unit =
    E.object <|
        [ ( "count", E.int unit.count )
        , ( "name", E.string unit.name )
        , ( "xp", E.int unit.xp )
        , ( "profile", encodeProfile unit.profile )
        , ( "equipment", E.list encodeEquipment unit.equipment )
        ]


decodeTreasury : Decoder Treasury
decodeTreasury =
    Decode.succeed Treasury
        |> required "gold" int
        |> required "wyrdstone" int
        |> optional "equipment" (list decodeEquipment) []


encodeTreasury : Treasury -> E.Value
encodeTreasury t =
    E.object <|
        [ ( "gold", E.int t.gold )
        , ( "wyrdstone", E.int t.wyrdstone )
        , ( "equipment", E.list encodeEquipment t.equipment )
        ]


decodeWarband : Decoder Warband
decodeWarband =
    Decode.succeed Warband
        |> required "name" string
        |> required "treasury" decodeTreasury
        |> required "units" (list decodeUnit)
        |> optional "notes" string ""


encodeWarband : Warband -> E.Value
encodeWarband warband =
    E.object <|
        [ ( "name", E.string warband.name )
        , ( "treasury", encodeTreasury warband.treasury )
        , ( "units", E.list encodeUnit warband.units )
        , ( "notes", E.string warband.notes )
        ]
