module Main exposing (main)

import Array exposing (Array)
import Browser
import Clock exposing (clock)
import Html as H exposing (Html)
import Html.Attributes as HA
import Logo
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


type alias ClockSetting msg =
    { logo : Svg msg
    , frequency : Int
    }


clockSettings : List (ClockSetting msg)
clockSettings =
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
    , ClockSetting Logo.accutron 360
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
    { time : Time.Posix
    , isMoving : Bool
    }


type alias Model =
    { zone : Time.Zone
    , clockStates : Array ClockState
    }


defaultClockState : ClockState
defaultClockState =
    ClockState (Time.millisToPosix 0) True


initialModel : Model
initialModel =
    { zone = Time.utc
    , clockStates =
        defaultClockState
            |> List.repeat (List.length clockSettings)
            |> Array.fromList
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch <|
        Task.perform AdjustTimeZone Time.here
            :: mapWithIndex (\i -> Task.perform (Tick i) Time.now) clockSettings
    )



-- UPDATE


type Msg
    = Tick Int Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleClock Int Bool


getClockState : Model -> Int -> ClockState
getClockState model index =
    Array.get index model.clockStates
        |> Maybe.withDefault defaultClockState


setClockState : Model -> Int -> ClockState -> Model
setClockState model index newState =
    { model | clockStates = Array.set index newState model.clockStates }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick index newTime ->
            let
                { isMoving } =
                    getClockState model index
            in
            ( setClockState model index (ClockState newTime isMoving)
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ToggleClock index moving ->
            let
                { time } =
                    getClockState model index
            in
            ( setClockState model index (ClockState time moving), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    clockSettings
        |> List.map (.frequency >> toFloat >> (/) 1000)
        |> List.indexedMap (\i t -> ( i, t ))
        |> List.filter
            (\( i, _ ) ->
                let
                    { isMoving } =
                        getClockState model i
                in
                isMoving
            )
        |> List.map (\( i, t ) -> Time.every t (Tick i))
        |> Sub.batch



-- VIEW


view : Model -> Html Msg
view model =
    H.main_ [ HA.style "display" "flex", HA.style "user-select" "none", HA.style "flex-wrap" "wrap" ]
        (mapWithIndex (\i -> renderClock i model) clockSettings)


defaultClockSetting : ClockSetting msg
defaultClockSetting =
    ClockSetting Logo.nothing 1


renderClock : Int -> Model -> Html Msg
renderClock index model =
    let
        { time, isMoving } =
            Array.get index model.clockStates
                |> Maybe.withDefault defaultClockState

        { logo, frequency } =
            clockSettings
                |> Array.fromList
                |> Array.get index
                |> Maybe.withDefault defaultClockSetting
    in
    clock logo frequency model.zone time isMoving (ToggleClock index)
