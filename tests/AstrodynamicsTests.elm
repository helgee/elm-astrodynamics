module AstrodynamicsTests exposing (almostEqualTest, cartesianToKeplerian, modTwoPiTest, moduloTest)

import Astrodynamics exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Vector3d as V3 exposing (Vector3d)


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
        keplerianElements =
            Astrodynamics.keplerian
                (V3.fromComponents ( 1131.34, -2282.343, 6672.423 ))
                (V3.fromComponents ( -5.64305, 4.30333, 2.42879 ))
                3.986004418e5
    in
    describe "Cartesian coordinates to Keplerian elements conversion"
        [ test "Semi-major axis" <|
            \_ ->
                keplerianElements.semiMajorAxis
                    |> Expect.within (Expect.Relative sqrtEps) 7200.470581180567
        , test "Eccentricity" <|
            \_ ->
                keplerianElements.eccentricity
                    |> Expect.within (Expect.Relative sqrtEps) 0.008100116890743586
        , test "Inclination" <|
            \_ ->
                keplerianElements.inclination
                    |> Expect.within (Expect.Relative sqrtEps) 1.7208944567902595
        , test "Right Ascension of Ascending Node" <|
            \_ ->
                keplerianElements.ascendingNode
                    |> Expect.within (Expect.Relative sqrtEps) 5.579892976386111
        , test "Argument of Pericenter" <|
            \_ ->
                keplerianElements.argumentOfPericenter
                    |> Expect.within (Expect.Relative sqrtEps) 1.2370820968712155
        , test "True Anomaly" <|
            \_ ->
                keplerianElements.trueAnomaly
                    |> Expect.within (Expect.Relative sqrtEps) 7.194559370904103e-5
        ]
