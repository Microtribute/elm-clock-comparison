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
    , ClockSetting Logo.rolex 250
    , ClockSetting Logo.vacheronConstantin 500
    ]


intervals : List Float
intervals =
    List.map (.frequency >> toFloat >> (/) 1000) clockSettings


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


type alias Model =
    { zone : Time.Zone
    , times : Array Time.Posix
    }


initialModel : Model
initialModel =
    { zone = Time.utc
    , times =
        Time.millisToPosix 0
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
    H.main_ [ HA.style "display" "flex", HA.style "user-select" "none", HA.style "flex-wrap" "wrap" ]
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

        { logo, frequency } =
            clockSettings
                |> Array.fromList
                |> Array.get index
                |> Maybe.withDefault (ClockSetting Logo.nothing 1)
    in
    clock logo frequency model.zone time
