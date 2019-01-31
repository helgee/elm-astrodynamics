module AstrodynamicsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Astrodynamics exposing (..)
import Math.Vector3 as V3 exposing (Vec3)


-- r =
--     (V3.vec3 1131.34 -2282.343 6672.423)


r : Vec3
r =
    (V3.vec3 1131.3392333984375 -2282.342529296875 6672.4228515625)


v : Vec3
v =
    (V3.vec3 -5.643049240112305 4.303330421447754 2.4287891387939453)



-- v =
--     (V3.vec3 -5.64305 4.30333 2.42879)


almostEqualTest : Test
almostEqualTest =
    describe "Test equality for floats"
        [ test "Close to 4" <|
            \_ ->
                Astrodynamics.almostEqual 4.00000000000001 4.0
                    |> Expect.true "Expect to be equal"
        , test "Close to 5" <|
            \_ ->
                Astrodynamics.almostEqual 5.0 4.999999999999993
                    |> Expect.true "Expect to be equal"
        , test "Is not almost equal" <|
            \_ ->
                Astrodynamics.almostEqual 4.000000002 4.00300002
                    |> Expect.false "Expect to be not equal"
        ]


moduloTest : Test
moduloTest =
    describe "Test the modulo function"
        [ test "9 modulo 5 is 4" <|
            \_ ->
                modulo 9 5
                    |> Expect.equal 4
        , test "-1 modulo 10 is 9" <|
            \_ ->
                modulo -1 10
                    |> Expect.equal 9
        ]


modTwoPiTest : Test
modTwoPiTest =
    describe "Test the modTwoPi function"
        [ test "(modTwoPi 0.2 * pi) == 0.2 * pi" <|
            \_ ->
                modTwoPi (0.2 * pi)
                    |> Expect.within (Expect.Relative sqrtEps) (0.2 * pi)
        , test "(modTwoPi 2.2 * pi) == 0.2 * pi" <|
            \_ ->
                modTwoPi (2.2 * pi)
                    |> Expect.within (Expect.Relative sqrtEps) (0.2 * pi)
        , test "(modTwoPi -0.2 * pi) == 1.8 * pi" <|
            \_ ->
                modTwoPi (-0.2 * pi)
                    |> Expect.within (Expect.Relative sqrtEps) (1.8 * pi)
        ]


cartesianToKeplerian : Test
cartesianToKeplerian =
    let
        ( semiMajorAxis, eccentricity, inclination, ascendingNode, argumentOfPericenter, trueAnomaly ) =
            Astrodynamics.keplerian r v 3.986004418e5

        ( r1, v1 ) =
            cartesian
                semiMajorAxis
                eccentricity
                inclination
                ascendingNode
                argumentOfPericenter
                trueAnomaly
                3.986004418e5

        a =
            Debug.log "r1, v1" ( r1, v1 )
    in
        describe "Cartesian coordinates to Keplerian elements conversion"
            [ test "Semi-major axis" <|
                \_ ->
                    semiMajorAxis
                        |> Expect.within (Expect.Relative sqrtEps) 7200.468558810962
            , test "Eccentricity" <|
                \_ ->
                    eccentricity
                        |> Expect.within (Expect.Relative sqrtEps) 0.008099895313266001
            , test "Inclination" <|
                \_ ->
                    inclination
                        |> Expect.within (Expect.Relative sqrtEps) 1.720894448147967
            , test "Right Ascension of Ascending Node" <|
                \_ ->
                    ascendingNode
                        |> Expect.within (Expect.Relative sqrtEps) 5.5798928830942165
            , test "Argument of Pericenter" <|
                \_ ->
                    argumentOfPericenter
                        |> Expect.within (Expect.Relative sqrtEps) 1.2370818617096708
            , test "True Anomaly" <|
                \_ ->
                    trueAnomaly
                        |> Expect.within (Expect.Relative sqrtEps) 7.22786371125197e-5
            , test "rx" <|
                \_ ->
                    (V3.getX r1)
                        |> Expect.within (Expect.Relative sqrtEps) (V3.getX r)
            , test "ry" <|
                \_ ->
                    (V3.getY r1)
                        |> Expect.within (Expect.Relative 1.0e-3) (V3.getY r)
            , test "rz" <|
                \_ ->
                    (V3.getZ r1)
                        |> Expect.within (Expect.Relative 1.0e-3) (V3.getZ r)
            , test "vx" <|
                \_ ->
                    (V3.getX v1)
                        |> Expect.within (Expect.Relative 1.0e-3) (V3.getX v)
            , test "vy" <|
                \_ ->
                    (V3.getY v1)
                        |> Expect.within (Expect.Relative 1.0e-3) (V3.getY v)
            , test "vz" <|
                \_ ->
                    (V3.getZ v1)
                        |> Expect.within (Expect.Relative 1.0e-3) (V3.getZ v)
            ]
