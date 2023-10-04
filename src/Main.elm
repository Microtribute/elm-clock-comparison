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
    { logo : Maybe (Svg msg)
    , frequency : Int
    }


frequencies : List Int
frequencies =
    [ 1, 2, 4, 7, 8, 16, 24, 32, 40, 48, 60, 72, 80, 120, 250 ]


clockSettings : List (ClockSetting msg)
clockSettings =
    [ ClockSetting (Just Logo.citizen) 1
    , ClockSetting (Just Logo.zenith) 2
    , ClockSetting (Just Logo.tissot) 4
    , ClockSetting (Just Logo.omega) 7
    , ClockSetting (Just Logo.seiko) 8
    , ClockSetting (Just Logo.bulova) 16
    , ClockSetting (Just Logo.breitling) 24
    , ClockSetting (Just Logo.titoni) 32
    , ClockSetting (Just Logo.hublot) 40
    , ClockSetting (Just Logo.iwc) 48
    , ClockSetting (Just Logo.orient) 60
    , ClockSetting (Just Logo.oris) 72
    , ClockSetting (Just Logo.frederiqueConstant) 80
    , ClockSetting (Just Logo.rado) 120
    , ClockSetting (Just Logo.rolex) 250
    , ClockSetting (Just Logo.vacheronConstantin) 500
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
                |> Maybe.withDefault (ClockSetting Nothing 1)
    in
    clock logo frequency model.zone time
