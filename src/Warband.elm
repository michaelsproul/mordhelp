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
    }


type alias Warband =
    { name : String, treasury : Treasury, units : List Unit }


defaultWeapon : Weapon
defaultWeapon =
    { name = "Unarmed strike", kind = Melee, strength = ModifierMod 0, rend = Nothing }


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


decodeEquipment : Decoder Equipment
decodeEquipment =
    Decode.succeed
        (\name kind strength rend ->
            EquipmentWeapon
                { name = name
                , kind = kind
                , strength = strength
                , rend = rend
                }
        )
        |> required "name" string
        |> required "kind" decodeWeaponKind
        |> required "strength" decodeModifier
        |> optional "rend" (maybe decodeModifier) Nothing


decodeUnit : Decoder Unit
decodeUnit =
    Decode.succeed Unit
        |> required "count" int
        |> required "name" string
        |> required "xp" int
        |> required "profile" decodeProfile
        |> required "equipment" (list decodeEquipment)


decodeTreasury : Decoder Treasury
decodeTreasury =
    Decode.succeed Treasury
        |> required "gold" int
        |> required "wyrdstone" int


decodeWarband : Decoder Warband
decodeWarband =
    Decode.succeed Warband
        |> required "name" string
        |> required "treasury" decodeTreasury
        |> required "units" (list decodeUnit)
