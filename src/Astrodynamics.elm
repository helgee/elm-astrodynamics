module Astrodynamics exposing
    ( almostEqual
    , almostEqualRtolAtol
    , eps
    , keplerian
    , modTwoPi
    , modulo
    , muEarth
    , sqrtEps
    )

import Direction3d
import Vector3d as V3 exposing (Vector3d)


modulo : Float -> Float -> Float
modulo a n =
    a - toFloat (floor (a / n)) * n


almostEqualRtolAtol : Float -> Float -> Float -> Float -> Bool
almostEqualRtolAtol rtol atol x y =
    x
        == y
        || not (isInfinite x)
        && not (isInfinite y)
        && abs (x - y)
        <= atol
        + rtol
        * max (abs x) (abs y)
        || (isNaN x && isNaN y)


almostEqual : Float -> Float -> Bool
almostEqual x y =
    almostEqualRtolAtol 0.0 (sqrt eps) x y


modTwoPi : Float -> Float
modTwoPi a =
    modulo a (2 * pi)


muEarth : Float
muEarth =
    3.986004418e5


eps : Float
eps =
    2.220446049250313e-16


sqrtEps : Float
sqrtEps =
    sqrt eps


angle : Vector3d -> Vector3d -> Maybe Float
angle a b =
    let
        dirA =
            V3.direction a

        dirB =
            V3.direction b
    in
    Maybe.map2 Direction3d.angleFrom dirA dirB


type alias KeplerianElements =
    { semiMajorAxis : Float
    , eccentricity : Float
    , inclination : Float
    , ascendingNode : Float
    , argumentOfPericenter : Float
    , trueAnomaly : Float
    }


keplerian : Vector3d -> Vector3d -> Float -> KeplerianElements
keplerian r v mu =
    let
        rNorm =
            V3.length r

        vNorm =
            V3.length v

        h =
            V3.crossProduct r v

        hNorm =
            V3.length h

        k =
            V3.fromComponents ( 0.0, 0.0, 1.0 )

        n =
            V3.crossProduct k h

        nNorm =
            V3.length n

        xi =
            vNorm ^ 2 / 2 - mu / rNorm

        eccentricityVector =
            V3.scaleBy (1.0 / mu) (V3.difference (V3.scaleBy (vNorm ^ 2 - mu / rNorm) r) (V3.scaleBy (V3.dotProduct r v) v))

        eccentricity =
            V3.length eccentricityVector

        inclination =
            Maybe.withDefault 0.0 (angle h k)

        isEquatorial =
            almostEqual (abs inclination) 0

        isCircular =
            almostEqual eccentricity 0

        semiMajorAxis =
            if isCircular then
                hNorm ^ 2 / mu

            else
                -mu / (2 * xi)

        ( ascendingNode, argumentOfPericenter, trueAnomaly ) =
            if isEquatorial && not isCircular then
                let
                    anoY =
                        V3.dotProduct h (V3.crossProduct eccentricityVector r) / hNorm
                in
                ( 0.0
                , modTwoPi (atan2 (V3.yComponent eccentricityVector) (V3.xComponent eccentricityVector))
                , modTwoPi (atan2 anoY (V3.dotProduct r eccentricityVector))
                )

            else if not isEquatorial && isCircular then
                let
                    anoY =
                        V3.dotProduct r (V3.crossProduct h n) / hNorm
                in
                ( modTwoPi (atan2 (V3.yComponent n) (V3.xComponent n))
                , 0.0
                , modTwoPi (atan2 anoY (V3.dotProduct r n))
                )

            else if isEquatorial && isCircular then
                ( 0.0
                , 0.0
                , modTwoPi (atan2 (V3.yComponent r) (V3.xComponent r))
                )

            else
                let
                    periY =
                        V3.dotProduct eccentricityVector (V3.crossProduct h n) / hNorm

                    anoY =
                        V3.dotProduct r (V3.crossProduct h eccentricityVector) / hNorm
                in
                ( modTwoPi (atan2 (V3.yComponent n) (V3.xComponent n))
                , modTwoPi (atan2 periY (V3.dotProduct eccentricityVector n))
                , modTwoPi (atan2 anoY (V3.dotProduct r eccentricityVector))
                )
    in
    { semiMajorAxis = semiMajorAxis
    , eccentricity = eccentricity
    , inclination = inclination
    , ascendingNode = ascendingNode
    , argumentOfPericenter = argumentOfPericenter
    , trueAnomaly = trueAnomaly
    }
