module Main exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


frequencies : List Int
frequencies =
    [ 1, 2, 4, 7, 8, 16, 24, 32, 40, 48, 60, 72, 80, 120, 250 ]


interval : Int -> Float
interval hz =
    1000 / toFloat hz


intervals : List Float
intervals =
    List.map interval frequencies


mapWithIndex : (Int -> b) -> List a -> List b
mapWithIndex f =
    List.indexedMap (\i _ -> f i)


steppedRange : Float -> Float -> Float -> List Float
steppedRange step lower upper =
    if lower >= upper then
        []

    else
        lower :: steppedRange step (lower + step) upper



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , times : Array Time.Posix
    }


initialModel : Model
initialModel =
    { zone = Time.utc
    , times =
        Time.millisToPosix 0
            |> List.repeat (List.length frequencies)
            |> Array.fromList
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch <|
        Task.perform AdjustTimeZone Time.here
            :: mapWithIndex (\i -> Task.perform (Tick i) Time.now) frequencies
    )



-- UPDATE


type Msg
    = Tick Int Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick index newTime ->
            ( { model | times = Array.set index newTime model.times }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    intervals
        |> List.indexedMap (\i t -> Time.every t (Tick i))
        |> Sub.batch



-- VIEW


view : Model -> Html Msg
view model =
    H.main_ [ HA.style "display" "flex", HA.style "flex-wrap" "wrap" ]
        (model.times
            |> Array.toList
            |> mapWithIndex (\i -> renderClock i model)
        )


renderClock : Int -> Model -> Html Msg
renderClock index model =
    let
        time =
            Array.get index model.times
                |> Maybe.withDefault (Time.millisToPosix 0)

        frequency =
            frequencies
                |> Array.fromList
                |> Array.get index
                |> Maybe.withDefault 0

        mSec =
            toFloat 1000

        mMin =
            toFloat 60000

        mHour =
            toFloat 3600000

        millis =
            toMillis model.zone time |> toFloat

        step =
            interval frequency

        h =
            toFloat (Time.toHour model.zone time)

        m =
            toFloat (Time.toMinute model.zone time)

        s =
            toFloat (Time.toSecond model.zone time)

        pmillis =
            h * mHour + m * mMin + s * mSec

        actualMillis =
            pmillis + toFloat (floor ((millis - pmillis) / step)) * step

        -- _ =
        --     Debug.log "aaa" <| String.fromFloat pmillis ++ ", " ++ String.fromFloat millis ++ ", " ++ String.fromFloat actualMillis
        hour =
            actualMillis / mHour

        minute =
            (actualMillis - toFloat (floor hour) * mHour) / mMin

        second =
            (actualMillis - (toFloat (floor hour) * mHour + toFloat (floor minute) * mMin)) / mSec

        drawHand : Int -> Float -> String -> Float -> Svg msg
        drawHand width length color turns =
            let
                t =
                    2 * pi * (turns - 0.25)

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

        drawMark : Float -> Html Msg
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
                    case integer n of
                        Just int ->
                            if 0 == remainderBy 5 int then
                                ( 1, 80, "red" )

                            else if 0 == remainderBy 1 int then
                                ( 1, 90, "blue" )

                            else
                                ( 1, 95, "blue" )

                        Nothing ->
                            ( 1, 95, "blue" )

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
    in
    H.section
        [ HA.style "border-radius" "20px"
        , HA.style "background-color" "#1293D8"
        , HA.style "padding" "24px 12px"
        , HA.style "margin" "4px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ svg
            [ viewBox "0 0 200 200"
            , width "200"
            , height "200"
            ]
            [ circle [ cx "100", cy "100", r "99", fill "#52a9d8", stroke "blue" ] []
            , Svg.text_ [ x "100", y "60", fontSize "14", fontFamily "sans-serif", fill "white", textAnchor "middle" ] [ Svg.text "CITIZEN" ]
            , Svg.text_ [ x "100", y "70", fontSize "6", fontFamily "sans-serif", fill "lightgray", textAnchor "middle" ] [ Svg.text "RADIO-CONTROLLED" ]
            , g [] <| List.map drawMark <| steppedRange 0.25 0 60
            , drawHand 4 40 "#fff" (hour / 12)
            , drawHand 4 60 "#efefef" (minute / 60)
            , drawHand 2 85 "yellow" (second / 60)
            , circle [ cx "100", cy "100", r "4", fill "gray" ] []
            ]
        , H.aside
            [ HA.style "font-size" "12px"
            , HA.style "color" "white"
            , HA.style "padding-top" "24px"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "display" "flex"
            ]
            [ "f = " ++ String.fromInt frequency ++ " bps" |> H.text ]
        ]


toMillis : Time.Zone -> Time.Posix -> Int
toMillis z t =
    let
        hh =
            Time.toHour z t

        mm =
            Time.toMinute z t

        ss =
            Time.toSecond z t

        mi =
            Time.toMillis z t
    in
    hh * 3600000 + mm * 60000 + ss * 1000 + mi
