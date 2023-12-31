module Attack exposing (rendByStrength, toHitBallistic, toHitByWs, toWoundByStrength)

import Warband exposing (Profile, Unit)


toHitByWs : Int -> Int -> Int
toHitByWs attackerWs defenderWs =
    if attackerWs > defenderWs then
        3

    else if defenderWs > 2 * attackerWs then
        5

    else
        4


toHitByProfile : Profile -> Profile -> Int
toHitByProfile attacker defender =
    toHitByWs attacker.weaponSkill defender.weaponSkill


toHitByUnit : Unit -> Unit -> Int
toHitByUnit attacker defender =
    toHitByProfile attacker.profile defender.profile


toWoundByStrength : Int -> Int -> Maybe Int
toWoundByStrength strength toughness =
    if toughness > strength + 3 then
        Nothing

    else if toughness == strength + 3 then
        Just 6

    else
        Just (max (4 - strength + toughness) 2)


rendByStrength : Int -> Int
rendByStrength strength =
    max (min (3 - strength) 0) -6


toHitBallistic : Int -> Int
toHitBallistic bs =
    7 - bs
