module Warband exposing (Equipment, Profile, Unit, UnitKind, WeaponKind, WeaponStrength)


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
        , strength : WeaponStrength
        }


type alias Unit =
    { count : Int
    , profile : Profile
    , equipment : List Equipment
    }
