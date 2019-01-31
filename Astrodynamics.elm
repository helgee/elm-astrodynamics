module Astrodynamics exposing (..)

import Math.Vector3 as V3 exposing (Vec3)
import Quaternion exposing (Quaternion)


modulo : Float -> Float -> Float
modulo a n =
    a - toFloat (floor (a / n)) * n


almostEqualRtolAtol : Float -> Float -> Float -> Float -> Bool
almostEqualRtolAtol rtol atol x y =
    (x == y)
        || (not (isInfinite x) && not (isInfinite y))
        && (abs (x - y) <= atol + rtol * max (abs x) (abs y))
        || (isNaN x && isNaN y)


almostEqual : Float -> Float -> Bool
almostEqual x y =
    almostEqualRtolAtol 0.0 (sqrt eps) x y


modTwoPi : Float -> Float
modTwoPi a =
    modulo a (2 * pi)


mu : Float
mu =
    3.986004418e5


eps : Float
eps =
    2.220446049250313e-16


sqrtEps : Float
sqrtEps =
    sqrt eps


angle : Vec3 -> Vec3 -> Float
angle a b =
    acos (V3.dot (V3.normalize a) (V3.normalize b))


keplerian : Vec3 -> Vec3 -> Float -> ( Float, Float, Float, Float, Float, Float )
keplerian r v mu =
    let
        rNorm =
            V3.length r

        vNorm =
            V3.length v

        h =
            V3.cross r v

        hNorm =
            V3.length h

        k =
            V3.vec3 0 0 1

        n =
            V3.cross k h

        nNorm =
            V3.length n

        xi =
            vNorm ^ 2 / 2 - mu / rNorm

        eccentricityVector =
            V3.scale (1 / mu) (V3.sub (V3.scale (vNorm ^ 2 - mu / rNorm) r) (V3.scale (V3.dot r v) v))

        eccentricity =
            V3.length eccentricityVector

        inclination =
            angle h k

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
                        (V3.dot h (V3.cross eccentricityVector r)) / hNorm
                in
                    ( 0
                    , modTwoPi (atan2 (V3.getY eccentricityVector) (V3.getX eccentricityVector))
                    , modTwoPi (atan2 anoY (V3.dot r eccentricityVector))
                    )
            else if not isEquatorial && isCircular then
                let
                    anoY =
                        (V3.dot r (V3.cross h n)) / hNorm
                in
                    ( modTwoPi (atan2 (V3.getY n) (V3.getX n))
                    , 0.0
                    , modTwoPi (atan2 anoY (V3.dot r n))
                    )
            else if isEquatorial && isCircular then
                ( 0
                , 0
                , modTwoPi (atan2 (V3.getY r) (V3.getX r))
                )
            else
                let
                    periY =
                        (V3.dot eccentricityVector (V3.cross h n)) / hNorm

                    anoY =
                        (V3.dot r (V3.cross h eccentricityVector)) / hNorm
                in
                    ( modTwoPi (atan2 (V3.getY n) (V3.getX n))
                    , modTwoPi (atan2 periY (V3.dot eccentricityVector n))
                    , modTwoPi (atan2 anoY (V3.dot r eccentricityVector))
                    )
    in
        ( semiMajorAxis
        , eccentricity
        , inclination
        , ascendingNode
        , argumentOfPericenter
        , trueAnomaly
        )


perifocal : Float -> Float -> Float -> Float -> ( Vec3, Vec3 )
perifocal semiLatus eccentricity trueAnomaly mu =
    let
        ca =
            cos trueAnomaly

        sa =
            sin trueAnomaly
    in
        ( (V3.vec3
            (semiLatus * ca / (1 + eccentricity * ca))
            (semiLatus * sa / (1 + eccentricity * ca))
            0
          )
        , (V3.vec3
            (-(sqrt (mu / semiLatus)) * sa)
            (sqrt (mu / semiLatus) * (eccentricity + ca))
            0
          )
        )


cartesian : Float -> Float -> Float -> Float -> Float -> Float -> Float -> ( Vec3, Vec3 )
cartesian semiMajorAxis eccentricity inclination ascendingNode argumentOfPericenter trueAnomaly mu =
    let
        semiLatus =
            if almostEqual eccentricity 0 then
                semiMajorAxis
            else
                semiMajorAxis * (1 - eccentricity ^ 2)

        ( rPqw, vPqw ) =
            perifocal semiLatus eccentricity trueAnomaly mu

        qPeri =
            Quaternion.fromAngleAxis argumentOfPericenter (V3.vec3 0 0 1)

        qInc =
            Quaternion.fromAngleAxis inclination (V3.vec3 1 0 0)

        qNode =
            Quaternion.fromAngleAxis ascendingNode (V3.vec3 0 0 1)

        q =
            Quaternion.multiply qNode (Quaternion.multiply qInc qPeri)

        b =
            Debug.log "q" q
    in
        ( Quaternion.rotate q rPqw
        , Quaternion.rotate q vPqw
        )
