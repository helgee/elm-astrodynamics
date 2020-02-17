module AstrodynamicsTests exposing
    ( almostEqualTest
    , cartesianToKeplerian
    , dateTest
    , modTwoPiTest
    , moduloTest
    , timeTest
    )

import AstroTime exposing (..)
import Astrodynamics exposing (..)
import Date exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Result
import Test exposing (..)
import Vector3d as V3 exposing (Vector3d)


unwrapDate : Result String Date -> Date
unwrapDate res =
    Result.withDefault julianEpochDate res


dateTest : Test
dateTest =
    let
        dayNumbers =
            [ -2451546
            , -2451545
            , -730122
            , -730121
            , -182554
            , -182553
            , -182552
            , -152385
            , -152384
            , -146039
            , -146038
            , -146037
            , -109514
            , -109513
            , -72990
            , -72989
            , -51546
            , -51545
            , -1
            , 0
            , 58
            , 59
            , 60
            ]

        dates =
            [ Date.fromYearMonthDay -4713 12 31
            , Date.fromYearMonthDay -4712 1 1
            , Date.fromYearMonthDay 0 12 31
            , Date.fromYearMonthDay 1 1 1
            , Date.fromYearMonthDay 1500 2 28
            , Date.fromYearMonthDay 1500 2 29
            , Date.fromYearMonthDay 1500 3 1
            , Date.fromYearMonthDay 1582 10 4
            , Date.fromYearMonthDay 1582 10 15
            , Date.fromYearMonthDay 1600 2 28
            , Date.fromYearMonthDay 1600 2 29
            , Date.fromYearMonthDay 1600 3 1
            , Date.fromYearMonthDay 1700 2 28
            , Date.fromYearMonthDay 1700 3 1
            , Date.fromYearMonthDay 1800 2 28
            , Date.fromYearMonthDay 1800 3 1
            , Date.fromYearMonthDay 1858 11 15
            , Date.fromYearMonthDay 1858 11 16
            , Date.fromYearMonthDay 1999 12 31
            , Date.fromYearMonthDay 2000 1 1
            , Date.fromYearMonthDay 2000 2 28
            , Date.fromYearMonthDay 2000 2 29
            , Date.fromYearMonthDay 2000 3 1
            ]
                |> List.map unwrapDate

        years =
            [ -4713
            , -4712
            , 0
            , 1
            , 1500
            , 1500
            , 1500
            , 1582
            , 1582
            , 1600
            , 1600
            , 1600
            , 1700
            , 1700
            , 1800
            , 1800
            , 1858
            , 1858
            , 1999
            , 2000
            , 2000
            , 2000
            , 2000
            ]

        months =
            [ 12
            , 1
            , 12
            , 1
            , 2
            , 2
            , 3
            , 10
            , 10
            , 2
            , 2
            , 3
            , 2
            , 3
            , 2
            , 3
            , 11
            , 11
            , 12
            , 1
            , 2
            , 2
            , 3
            ]

        days =
            [ 31
            , 1
            , 31
            , 1
            , 28
            , 29
            , 1
            , 4
            , 15
            , 28
            , 29
            , 1
            , 28
            , 1
            , 28
            , 1
            , 15
            , 16
            , 31
            , 1
            , 28
            , 29
            , 1
            ]
    in
    describe "Test Date module"
        [ test "This is the last day in the proleptic Julian calendar" <|
            \_ ->
                fromDayNumber -730122
                    |> unwrapDate
                    |> getCalendar
                    |> Expect.equal ProlepticJulianCalendar
        , test "This is the first day in the Julian calendar" <|
            \_ ->
                fromDayNumber -730121
                    |> unwrapDate
                    |> getCalendar
                    |> Expect.equal JulianCalendar
        , test "This is the last day in the Julian calendar" <|
            \_ ->
                fromDayNumber -152385
                    |> unwrapDate
                    |> getCalendar
                    |> Expect.equal JulianCalendar
        , test "This is the first day in the Gregorian calendar" <|
            \_ ->
                fromDayNumber -152384
                    |> unwrapDate
                    |> getCalendar
                    |> Expect.equal GregorianCalendar
        , test "Years are equal" <|
            \_ ->
                dayNumbers
                    |> List.map fromDayNumber
                    |> List.map unwrapDate
                    |> List.map getYear
                    |> Expect.equalLists years
        , test "Months are equal" <|
            \_ ->
                dayNumbers
                    |> List.map fromDayNumber
                    |> List.map unwrapDate
                    |> List.map getMonth
                    |> Expect.equalLists months
        , test "Days are equal" <|
            \_ ->
                dayNumbers
                    |> List.map fromDayNumber
                    |> List.map unwrapDate
                    |> List.map getDay
                    |> Expect.equalLists days
        , test "J2000 days are equal" <|
            \_ ->
                dates
                    |> List.map getJ2000
                    |> Expect.equalLists dayNumbers
        , test "To String" <|
            \_ ->
                Date.fromYearMonthDay 2000 1 1
                    |> unwrapDate
                    |> Date.toString
                    |> Expect.equal "2000-01-01"
        ]


unwrapTime : Result String Time -> Time
unwrapTime res =
    case res of
        Err _ ->
            noon

        Ok time ->
            time


timeTest : Test
timeTest =
    describe "Test Time module"
        [ test "Test hour check" <|
            \_ ->
                AstroTime.fromHourMinuteSecond 24 59 59.0
                    |> Expect.err
        , test "Test minute check" <|
            \_ ->
                AstroTime.fromHourMinuteSecond 23 60 59.0
                    |> Expect.err
        , test "Test second check" <|
            \_ ->
                AstroTime.fromHourMinuteSecond 23 59 61.0
                    |> Expect.err
        , test "To String" <|
            \_ ->
                AstroTime.fromHourMinuteSecond 13 4 31.415926535897
                    |> unwrapTime
                    |> AstroTime.toString
                    |> Expect.equal "13:04:31.416"
        ]


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
        r =
            V3.fromComponents ( 1131.34, -2282.343, 6672.423 )

        v =
            V3.fromComponents ( -5.64305, 4.30333, 2.42879 )

        keplerianElements =
            Astrodynamics.keplerian r v muEarth

        ( r1, v1 ) =
            Astrodynamics.cartesian keplerianElements muEarth
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
        , test "rx" <|
            \_ ->
                V3.xComponent r1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.xComponent r)
        , test "ry" <|
            \_ ->
                V3.yComponent r1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.yComponent r)
        , test "rz" <|
            \_ ->
                V3.zComponent r1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.zComponent r)
        , test "vx" <|
            \_ ->
                V3.xComponent v1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.xComponent v)
        , test "vy" <|
            \_ ->
                V3.yComponent v1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.yComponent v)
        , test "vz" <|
            \_ ->
                V3.zComponent v1
                    |> Expect.within (Expect.Relative sqrtEps) (V3.zComponent v)
        ]
