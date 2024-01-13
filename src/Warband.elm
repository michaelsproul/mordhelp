module Warband exposing
    ( Equipment(..)
    , Modifier(..)
    , Profile
    , Unit
    , UnitKind
    , Warband
    , WeaponKind(..)
    , WeaponStrength
    , decodeWarband
    , defaultWeapon
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


type Modifier
    = ModifierAbs Int
    | ModifierMod Int


type alias WeaponStrength =
    Modifier


type alias WeaponRend =
    Modifier


type alias Weapon =
    { name : String
    , kind : WeaponKind
    , strength : WeaponStrength
    , rend : Maybe WeaponRend
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
    }


type alias Treasury =
    { gold : Int
    , wyrdstone : Int
    , equipment : List Equipment
    }


type alias Warband =
    { name : String, treasury : Treasury, units : List Unit }


defaultWeapon : Weapon
defaultWeapon =
    { name = "Unarmed strike"
    , kind = Melee
    , strength = ModifierMod 0
    , rend = Nothing
    , specialRules = Nothing
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


decodeModifier : Decoder Modifier
decodeModifier =
    string
        |> Decode.andThen
            (\s ->
                if String.startsWith "mod" s then
                    case String.toInt (String.dropLeft 3 s) of
                        Just n ->
                            Decode.succeed (ModifierMod n)

                        Nothing ->
                            Decode.fail <| "invalid modifier: " ++ s

                else if String.startsWith "abs" s then
                    case String.toInt (String.dropLeft 3 s) of
                        Just n ->
                            Decode.succeed (ModifierAbs n)

                        Nothing ->
                            Decode.fail <| "invalid absolute modifier: " ++ s

                else
                    Decode.fail <| "invalid modifier: " ++ s
            )


encodeModifier : Modifier -> E.Value
encodeModifier modifier =
    case modifier of
        ModifierAbs n ->
            E.string <| "abs" ++ String.fromInt n

        ModifierMod n ->
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
        |> optional "rend" (maybe decodeModifier) Nothing
        |> optional "specialRules" (maybe (list string)) Nothing


encodeEquipment : Equipment -> E.Value
encodeEquipment equipment =
    case equipment of
        EquipmentWeapon w ->
            encodeWeapon w


encodeWeapon : Weapon -> E.Value
encodeWeapon weapon =
    E.object <|
        [ ( "name", E.string weapon.name )
        , ( "kind", encodeWeaponKind weapon.kind )
        , ( "strength", encodeModifier weapon.strength )
        ]
            ++ encodeMaybeField "rend" weapon.rend encodeModifier
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


encodeWarband : Warband -> E.Value
encodeWarband warband =
    E.object <|
        [ ( "name", E.string warband.name )
        , ( "treasury", encodeTreasury warband.treasury )
        , ( "units", E.list encodeUnit warband.units )
        ]
