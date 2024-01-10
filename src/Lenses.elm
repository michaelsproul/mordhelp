module Lenses exposing (movement, unitProfile)

import Accessors exposing (makeOneToOne)
import Warband exposing (Profile, Unit, Warband)


profileName =
    makeOneToOne
        .name
        (\change record -> { record | name = change record.name })


kind =
    makeOneToOne
        .kind
        (\change record -> { record | kind = change record.kind })


movement =
    makeOneToOne
        .movement
        (\change record -> { record | movement = change record.movement })


weaponSkill =
    makeOneToOne
        .weaponSkill
        (\change record -> { record | weaponSkill = change record.weaponSkill })


ballisticSkill =
    makeOneToOne
        .ballisticSkill
        (\change record -> { record | ballisticSkill = change record.ballisticSkill })


strength =
    makeOneToOne
        .strength
        (\change record -> { record | strength = change record.strength })


toughness =
    makeOneToOne
        .toughness
        (\change record -> { record | toughness = change record.toughness })


wounds =
    makeOneToOne
        .wounds
        (\change record -> { record | wounds = change record.wounds })


initiative =
    makeOneToOne
        .initiative
        (\change record -> { record | initiative = change record.initiative })


attacks =
    makeOneToOne
        .attacks
        (\change record -> { record | attacks = change record.attacks })


leadership =
    makeOneToOne
        .leadership
        (\change record -> { record | leadership = change record.leadership })


unitProfile =
    makeOneToOne
        .profile
        (\change record -> { record | profile = change record.profile })
