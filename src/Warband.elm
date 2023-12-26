module Warband exposing (Equipment, Profile, Unit, UnitKind, Warband, WeaponKind, WeaponStrength, decodeWarband)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


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
    }


type UnitKind
    = Hero
    | Henchman


type WeaponKind
    = Melee
    | Ballistic


type WeaponStrength
    = StrengthConst Int
    | StrengthMod Int


type Equipment
    = Weapon
        { name : String
        , kind : WeaponKind
        , strength : WeaponStrength
        }


type alias Unit =
    { count : Int
    , profile : Profile
    , equipment : List Equipment
    }


type alias Warband =
    { units : List Unit }


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


decodeWeaponKind : Decoder WeaponKind
decodeWeaponKind =
    Decode.oneOf
        [ string |> Decode.andThen (decodeLiteralAsValue "melee" Melee)
        , string |> Decode.andThen (decodeLiteralAsValue "ballistic" Ballistic)
        ]


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


decodeWeaponStrength : Decoder WeaponStrength
decodeWeaponStrength =
    string
        |> Decode.andThen
            (\s ->
                if String.startsWith "+" s then
                    case String.toInt (String.dropLeft 1 s) of
                        Just n ->
                            Decode.succeed (StrengthMod n)

                        Nothing ->
                            Decode.fail <| "invalid strength mod: " ++ s

                else
                    case String.toInt s of
                        Just n ->
                            Decode.succeed (StrengthConst n)

                        Nothing ->
                            Decode.fail <| "invalid strength const: " ++ s
            )


decodeEquipment : Decoder Equipment
decodeEquipment =
    Decode.succeed (\name kind strength -> Weapon { name = name, kind = kind, strength = strength })
        |> required "name" string
        |> required "kind" decodeWeaponKind
        |> required "strength" decodeWeaponStrength


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed Unit
        |> required "count" int
        |> required "profile" decodeProfile
        |> required "equipment" (list decodeEquipment)


decodeWarband : Decoder Warband
decodeWarband =
    Decode.succeed Warband
        |> required "units" (list decodeUnit)
