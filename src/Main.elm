module Main exposing (main)

import Array exposing (Array)
import Browser
import Clock exposing (clock)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Logo exposing (Logo)
import LogoDict as Logos
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


type alias ClockSetting =
    { logo : Logo
    , frequency : Int
    }


defaultClockSetting : ClockSetting
defaultClockSetting =
    ClockSetting Logo.nothing 1


defaultClockSettings : List ClockSetting
defaultClockSettings =
    [ ClockSetting Logo.citizen 1
    , ClockSetting Logo.zenith 2
    , ClockSetting Logo.tissot 4
    , ClockSetting Logo.omega 7
    , ClockSetting Logo.seiko 8
    , ClockSetting Logo.bulova 16
    , ClockSetting Logo.breitling 24
    , ClockSetting Logo.titoni 32
    , ClockSetting Logo.hublot 40
    , ClockSetting Logo.iwc 48
    , ClockSetting Logo.orient 60
    , ClockSetting Logo.oris 72
    , ClockSetting Logo.frederiqueConstant 80
    , ClockSetting Logo.rado 120
    , ClockSetting Logo.rolex 256
    , ClockSetting Logo.longines 360
    , ClockSetting Logo.vacheronConstantin 480
    , ClockSetting Logo.girardPerregaux 500
    , ClockSetting Logo.patekPhilippe 1000
    ]


mapWithIndex : (Int -> b) -> List a -> List b
mapWithIndex f =
    List.indexedMap (\i _ -> f i)



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


type alias ClockState =
    { setting : ClockSetting
    , time : Time.Posix
    , isMoving : Bool
    }


type alias Model =
    { zone : Time.Zone
    , clockStates : Array ClockState
    , newClock : ClockSetting
    }


defaultClockState : ClockState
defaultClockState =
    ClockState defaultClockSetting (Time.millisToPosix 0) True


initialModel : Model
initialModel =
    { zone = Time.utc
    , clockStates =
        defaultClockSettings
            |> List.map (\setting -> { defaultClockState | setting = setting })
            |> Array.fromList
    , newClock = defaultClockSetting
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch <|
        Task.perform AdjustTimeZone Time.here
            :: List.indexedMap (\i _ -> adjustTime i) defaultClockSettings
    )



-- UPDATE


type Msg
    = Tick Int Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleClock Int Bool
    | InputFrequency String
    | ChooseLogo String
    | AddClock


getClockState : Model -> Int -> ClockState
getClockState model index =
    Array.get index model.clockStates
        |> Maybe.withDefault defaultClockState


setClockState : Model -> Int -> ClockState -> Model
setClockState model index newState =
    { model | clockStates = Array.set index newState model.clockStates }


fixFrequency : String -> Int
fixFrequency input =
    let
        val =
            input |> String.toInt |> Maybe.withDefault 1
    in
    if val > 1000 then
        1000

    else if val < 1 then
        1

    else
        val


adjustTime : Int -> Cmd Msg
adjustTime index =
    Task.perform (Tick index) Time.now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick index newTime ->
            let
                clockState =
                    getClockState model index
            in
            ( setClockState model index { clockState | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ToggleClock index moving ->
            let
                clockState =
                    getClockState model index
            in
            ( setClockState model index { clockState | isMoving = moving }, Cmd.none )

        InputFrequency freq ->
            ( { model | newClock = ClockSetting model.newClock.logo (fixFrequency freq) }, Cmd.none )

        ChooseLogo key ->
            ( { model | newClock = ClockSetting (Logos.get key) model.newClock.frequency }, Cmd.none )

        AddClock ->
            ( { model
                | newClock = defaultClockSetting
                , clockStates = Array.push { defaultClockState | setting = model.newClock } model.clockStates
              }
            , adjustTime (Array.length model.clockStates)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.clockStates
        |> Array.toList
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, { isMoving } ) -> isMoving)
        |> List.map (\( i, { setting } ) -> ( i, setting.frequency ))
        |> List.map (\( i, f ) -> ( i, f |> toFloat |> (/) 1000 ))
        |> List.map (\( i, t ) -> Time.every t (Tick i))
        |> Sub.batch



-- VIEW


view : Model -> Html Msg
view model =
    H.main_ []
        [ renderForm model
        , renderClocks model
        ]


renderForm : Model -> Html Msg
renderForm model =
    H.section [ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "padding" "10px" ]
        [ H.div []
            [ H.label [ HA.for "logos" ] [ H.text "Choose Logo: " ]
            , H.select [ HA.id "logos", onInput ChooseLogo ] <|
                List.map (\name -> renderOption name (name == model.newClock.logo.name)) Logos.options
            ]
        , H.div []
            [ H.label [ HA.for "frequency" ] [ H.text "Frequency: " ]
            , H.input
                [ HA.type_ "number"
                , HA.id "frequency"
                , HA.size 3
                , HA.value (String.fromInt model.newClock.frequency)
                , onInput InputFrequency
                ]
                []
            ]
        , H.div []
            [ H.button [ HA.type_ "submit", onClick AddClock ] [ H.text "Add " ] ]
        ]


renderOption : String -> Bool -> Html Msg
renderOption name selected =
    H.option [ HA.value name, HA.selected selected ] [ H.text name ]


renderClocks : Model -> Html Msg
renderClocks model =
    model.clockStates
        |> Array.toList
        |> List.indexedMap
            (\i state ->
                renderClock state model.zone (ToggleClock i)
            )
        |> H.section [ HA.style "display" "flex", HA.style "user-select" "none", HA.style "flex-wrap" "wrap" ]


renderClock : ClockState -> Time.Zone -> (Bool -> Msg) -> Html Msg
renderClock { time, isMoving, setting } zone switcher =
    let
        { logo, frequency } =
            setting
    in
    H.section
        [ HA.style "padding" "10px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ clock logo frequency zone time isMoving
        , H.aside
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "padding-top" "10px"
            ]
            [ H.a
                [ onClick << switcher <| not isMoving
                , HA.style "font-size" "8px"
                , HA.style "font-family" "sans-serif"
                , HA.style "cursor" "pointer"
                ]
                [ text
                    << String.toUpper
                  <|
                    if isMoving then
                        "Switch Off"

                    else
                        "Switch On"
                ]
            ]
        ]
