module Lenses exposing (..)

import Accessors exposing (makeOneToOne)
import Warband exposing (Equipment(..), Profile, Unit, Warband)


profileName =
    makeOneToOne
        .name
        (\change record -> { record | name = change record.name })


profileKind =
    makeOneToOne
        .kind
        (\change record -> { record | kind = change record.kind })


profileMovement =
    makeOneToOne
        .movement
        (\change record -> { record | movement = change record.movement })


profileWeaponSkill =
    makeOneToOne
        .weaponSkill
        (\change record -> { record | weaponSkill = change record.weaponSkill })


profileBallisticSkill =
    makeOneToOne
        .ballisticSkill
        (\change record -> { record | ballisticSkill = change record.ballisticSkill })


profileStrength =
    makeOneToOne
        .strength
        (\change record -> { record | strength = change record.strength })


profileToughness =
    makeOneToOne
        .toughness
        (\change record -> { record | toughness = change record.toughness })


profileWounds =
    makeOneToOne
        .wounds
        (\change record -> { record | wounds = change record.wounds })


profileInitiative =
    makeOneToOne
        .initiative
        (\change record -> { record | initiative = change record.initiative })


profileAttacks =
    makeOneToOne
        .attacks
        (\change record -> { record | attacks = change record.attacks })


profileLeadership =
    makeOneToOne
        .leadership
        (\change record -> { record | leadership = change record.leadership })


unitProfile =
    makeOneToOne
        .profile
        (\change record -> { record | profile = change record.profile })


unitName =
    makeOneToOne
        .name
        (\change record -> { record | name = change record.name })


unitCount =
    makeOneToOne
        .count
        (\change record -> { record | count = change record.count })


unitXp =
    makeOneToOne
        .xp
        (\change record -> { record | xp = change record.xp })


unitEquipment =
    makeOneToOne
        .equipment
        (\change record -> { record | equipment = change record.equipment })


warbandName =
    makeOneToOne
        .name
        (\change record -> { record | name = change record.name })


warbandTreasury =
    makeOneToOne
        .treasury
        (\change record -> { record | treasury = change record.treasury })


warbandNotes =
    makeOneToOne
        .notes
        (\change record -> { record | notes = change record.notes })


treasuryGold =
    makeOneToOne
        .gold
        (\change record -> { record | gold = change record.gold })


treasuryWyrdstone =
    makeOneToOne
        .wyrdstone
        (\change record -> { record | wyrdstone = change record.wyrdstone })


equipmentWeapon =
    makeOneToOne
        (\(EquipmentWeapon w) -> w)
        (\change (EquipmentWeapon w) -> EquipmentWeapon (change w))


weaponName =
    makeOneToOne
        .name
        (\change record -> { record | name = change record.name })
