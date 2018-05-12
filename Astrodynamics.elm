module Astrodynamics exposing (..)

import Vector3 as V3 exposing (Float3)


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


mu : Float
mu =
    3.986004418e5


eps : Float
eps =
    2.220446049250313e-16


sqrtEps : Float
sqrtEps =
    sqrt eps


keplerian : Float3 -> Float3 -> Float -> ( Float, Float, Float, Float, Float, Float )
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
            ( 0, 0, 1 )

        n =
            V3.cross k h

        nNorm =
            V3.length n

        xi =
            vNorm ^ 2 / 2 - mu / rNorm

        eccentricityVector =
            V3.divideBy mu (V3.sub (V3.scale (vNorm ^ 2 - mu / rNorm) r) (V3.scale (V3.dot r v) v))

        eccentricity =
            V3.length eccentricityVector

        inclination =
            V3.angle h k

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
