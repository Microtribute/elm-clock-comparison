module Clock exposing (clock)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


mSec : Float
mSec =
    toFloat 1000


mMin : Float
mMin =
    toFloat 60000


mHour : Float
mHour =
    toFloat 3600000


anglify : Float -> Float
anglify turns =
    2 * pi * (turns - 0.25)


steppedRange : Float -> Float -> Float -> List Float
steppedRange step lower upper =
    if lower >= upper then
        []

    else
        lower :: steppedRange step (lower + step) upper


drawTriangularHandle : Float -> Float -> String -> Float -> Svg msg
drawTriangularHandle width length color turns =
    let
        angle =
            anglify turns

        xEnd =
            100 + length * cos angle

        yEnd =
            100 + length * sin angle

        xLeft =
            100 + width * cos (angle - pi / 2)

        yLeft =
            100 + width * sin (angle - pi / 2)

        xRight =
            100 + width * cos (angle + pi / 2)

        yRight =
            100 + width * sin (angle + pi / 2)
    in
    g []
        [ polygon [ points (String.join " " [ String.fromFloat xLeft ++ "," ++ String.fromFloat yLeft, String.fromFloat xEnd ++ "," ++ String.fromFloat yEnd, String.fromFloat xRight ++ "," ++ String.fromFloat yRight ]), fill color ] []
        , circle [ cx (String.fromFloat xEnd), cy (String.fromFloat yEnd), r (String.fromFloat (width / 2)), fill color ] []
        ]


drawTrepozoidalHand : ( Float, Float ) -> Float -> String -> Float -> Svg msg
drawTrepozoidalHand ( lower, upper ) length color turns =
    let
        angle =
            anglify turns

        xEnd =
            100 + length * cos angle

        yEnd =
            100 + length * sin angle

        xLeft =
            100 + upper * cos (angle - pi / 2)

        yLeft =
            100 + upper * sin (angle - pi / 2)

        xRight =
            100 + upper * cos (angle + pi / 2)

        yRight =
            100 + upper * sin (angle + pi / 2)

        xEndLeft =
            xEnd + lower * cos (angle - pi / 2)

        yEndLeft =
            yEnd + lower * sin (angle - pi / 2)

        xEndRight =
            xEnd + lower * cos (angle + pi / 2)

        yEndRight =
            yEnd + lower * sin (angle + pi / 2)
    in
    polygon
        [ points (String.join " " [ String.fromFloat xLeft ++ "," ++ String.fromFloat yLeft, String.fromFloat xEndLeft ++ "," ++ String.fromFloat yEndLeft, String.fromFloat xEndRight ++ "," ++ String.fromFloat yEndRight, String.fromFloat xRight ++ "," ++ String.fromFloat yRight ])
        , fill color
        ]
        []


drawHand : Int -> Float -> String -> Float -> Svg msg
drawHand width length color turns =
    let
        t =
            anglify turns

        x =
            100 + length * cos t

        y =
            100 + length * sin t
    in
    line
        [ x1 "100"
        , y1 "100"
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke color
        , strokeWidth (String.fromInt width)
        , strokeLinecap "round"
        ]
        []


clockLogoText : Svg msg
clockLogoText =
    g []
        [ Svg.text_ [ x "100", y "60", fontSize "14", fontFamily "sans-serif", fill "white", textAnchor "middle" ] [ Svg.text "CITIZEN" ]
        , Svg.text_ [ x "100", y "70", fontSize "6", fontFamily "sans-serif", fill "lightgray", textAnchor "middle" ] [ Svg.text "RADIO-CONTROLLED" ]
        ]


drawMark : Float -> Html msg
drawMark n =
    let
        integer : Float -> Maybe Int
        integer f =
            let
                ff =
                    floor f
            in
            if f == toFloat ff then
                Just ff

            else
                Nothing

        ( thickness, length, strokeColor ) =
            integer n
                |> Maybe.andThen
                    (\int ->
                        if 0 == remainderBy 5 int then
                            Just ( 1, 80, "darkred" )

                        else if 0 == remainderBy 1 int then
                            Just ( 1, 90, "gray" )

                        else
                            Nothing
                    )
                |> Maybe.withDefault ( 1, 95, "gray" )

        angle =
            pi * 2 * (n / 60)

        xStart =
            100 + length * cos angle

        yStart =
            100 + length * sin angle

        xEnd =
            100 + 98 * cos angle

        yEnd =
            100 + 98 * sin angle
    in
    line
        [ strokeWidth <| String.fromFloat thickness
        , x1 <| String.fromFloat xStart
        , y1 <| String.fromFloat yStart
        , x2 <| String.fromFloat xEnd
        , y2 <| String.fromFloat yEnd
        , stroke strokeColor
        ]
        []


clock : Svg msg -> Int -> Time.Zone -> Time.Posix -> Bool -> (Bool -> msg) -> Html msg
clock logo frequency zone time moving messenger =
    let
        millis =
            toMillis zone time

        step =
            1000 / toFloat frequency

        { hour, minute, second } =
            components zone time

        pmillis =
            hour * mHour + minute * mMin + second * mSec

        dc =
            pmillis + toFloat (floor ((millis - pmillis) / step)) * step

        dh =
            dc / mHour

        dm =
            (dc - toFloat (floor dh) * mHour) / mMin

        ds =
            (dc - (toFloat (floor dh) * mHour + toFloat (floor dm) * mMin)) / mSec
    in
    H.section
        [ HA.style "padding" "12px"
        , HA.style "margin" "4px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ svg
            [ viewBox "0 0 200 200"
            , width "200"
            , height "200"
            ]
            [ Svg.node "defs"
                []
                [ Svg.node "radialGradient"
                    [ Svg.Attributes.id "gradient"
                    , Svg.Attributes.cx "50%"
                    , Svg.Attributes.cy "50%"
                    , Svg.Attributes.r "50%"
                    , Svg.Attributes.fx "50%"
                    , Svg.Attributes.fy "50%"
                    ]
                    [ Svg.node "stop"
                        [ Svg.Attributes.offset "0%"
                        , Svg.Attributes.style "stop-color:rgba(255, 255, 255, 0.15); stop-opacity:1"
                        ]
                        []
                    , Svg.node "stop"
                        [ Svg.Attributes.offset "100%"
                        , Svg.Attributes.style "stop-color:rgba(255, 255, 255, 0.1); stop-opacity:0"
                        ]
                        []
                    ]
                ]
            , circle
                [ cx "100"
                , cy "100"
                , r "99"
                , fill <|
                    if moving then
                        "white"

                    else
                        "lightgray"
                , stroke "gray"
                ]
                []
            , g [] <| List.map drawMark <| steppedRange 0.25 0 60
            , logo
            , Svg.text_
                [ x "100"
                , y "150"
                , fontSize "8"
                , fontFamily "sans-serif"
                , fill "gray"
                , textAnchor "middle"
                ]
                [ Svg.text <| "- " ++ String.fromInt frequency ++ " bps -" ]

            -- , drawHand 4 40 "#fff" (hour / 12)
            -- , drawHand 4 60 "#efefef" (minute / 60)
            -- , drawHand 2 85 "yellow" (second / 60)
            -- , drawTriangularHandle 2 40 "#fff" (hour / 12)
            -- , drawTriangularHandle 2 60 "#efefef" (minute / 60)
            -- , drawTriangularHandle 1 85 "yellow" (second / 60)
            , drawTrepozoidalHand ( 1.5, 3 ) 40 "#357ca3" (dh / 12)
            , drawTrepozoidalHand ( 1, 3 ) 60 "gray" (dm / 60)
            , drawTrepozoidalHand ( 1, 2 ) 85 "#eb4351" (ds / 60)
            , circle [ cx "100", cy "100", r "4", fill "#eb4351" ] []
            ]
        , H.aside
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "padding-top" "12px"
            ]
            [ H.a
                [ onClick (messenger (not moving))
                , HA.style "font-size" "8px"
                , HA.style "font-family" "sans-serif"
                , HA.style "cursor" "pointer"
                ]
                [ text <|
                    if moving then
                        "Switch Off"

                    else
                        "Switch On"
                ]
            ]
        ]


components : Time.Zone -> Time.Posix -> { hour : Float, minute : Float, second : Float, milli : Float }
components z t =
    { hour = toFloat <| Time.toHour z t
    , minute = toFloat <| Time.toMinute z t
    , second = toFloat <| Time.toSecond z t
    , milli = toFloat <| Time.toMillis z t
    }


toMillis : Time.Zone -> Time.Posix -> Float
toMillis z t =
    let
        { hour, minute, second, milli } =
            components z t
    in
    hour * mHour + minute * mMin + second * mSec + milli
