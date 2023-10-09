module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
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


type alias ClockState =
    { setting : ClockSetting
    , time : Time.Posix
    , isMoving : Bool
    }


type alias Model =
    { zone : Time.Zone
    , clockStates : Array ClockState
    , form : ClockForm
    }


type alias ClockForm =
    { setting : ClockSetting, index : Maybe Int }


type Msg
    = Tick Int Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleClock Int Bool
    | InputFrequency String
    | ChooseLogo String
    | AddClock
    | DeleteClock Int
    | EditClock Int
    | SaveClock Int


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


defaultClockState : ClockState
defaultClockState =
    ClockState defaultClockSetting (Time.millisToPosix 0) True


defaultClockForm : ClockForm
defaultClockForm =
    ClockForm defaultClockSetting Nothing


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


initialModel : Model
initialModel =
    { zone = Time.utc
    , clockStates =
        defaultClockSettings
            |> List.map (\setting -> { defaultClockState | setting = setting })
            |> Array.fromList
    , form = defaultClockForm
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch <|
        Task.perform AdjustTimeZone Time.here
            :: List.indexedMap (\i _ -> adjustTime i) defaultClockSettings
    )



-- UPDATE


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
update msg ({ clockStates, form } as model) =
    let
        getClockState : Int -> ClockState
        getClockState index =
            Array.get index clockStates
                |> Maybe.withDefault defaultClockState

        setClockState : Int -> ClockState -> Array ClockState
        setClockState index newState =
            Array.set index newState clockStates
    in
    case msg of
        Tick index newTime ->
            let
                clockState =
                    getClockState index
            in
            ( { model | clockStates = setClockState index { clockState | time = newTime } }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        ToggleClock index moving ->
            let
                clockState =
                    getClockState index
            in
            ( { model | clockStates = setClockState index { clockState | isMoving = moving } }, Cmd.none )

        InputFrequency freq ->
            ( { model | form = { form | setting = ClockSetting model.form.setting.logo (fixFrequency freq) } }, Cmd.none )

        ChooseLogo key ->
            ( { model | form = { form | setting = ClockSetting (Logos.get key) model.form.setting.frequency } }, Cmd.none )

        AddClock ->
            ( { model
                | form = defaultClockForm
                , clockStates = Array.push { defaultClockState | setting = model.form.setting } model.clockStates
              }
            , adjustTime (Array.length model.clockStates)
            )

        DeleteClock index ->
            ( { model | clockStates = Array.removeAt index model.clockStates }, Cmd.none )

        EditClock index ->
            let
                { setting } =
                    getClockState index
            in
            ( { model | form = ClockForm setting <| Just index }, Cmd.none )

        SaveClock index ->
            ( { model | form = defaultClockForm, clockStates = Array.update index (\state -> { state | setting = model.form.setting }) model.clockStates }, Cmd.none )


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
    let
        indexBeingEdited =
            Maybe.withDefault -1 model.form.index
    in
    H.section [ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "padding" "10px" ]
        [ H.div []
            [ H.label [ HA.for "logos" ] [ H.text "Choose Logo: " ]
            , H.select [ HA.id "logos", onInput ChooseLogo ] <|
                List.map (\name -> renderOption name (name == model.form.setting.logo.name)) Logos.options
            ]
        , H.div []
            [ H.label [ HA.for "frequency" ] [ H.text "Frequency: " ]
            , H.input
                [ HA.type_ "number"
                , HA.id "frequency"
                , HA.size 3
                , HA.value (String.fromInt model.form.setting.frequency)
                , onInput InputFrequency
                ]
                []
            ]
        , H.div []
            [ H.button
                [ HA.type_ "submit"
                , onClick <|
                    if indexBeingEdited == -1 then
                        AddClock

                    else
                        SaveClock indexBeingEdited
                ]
                [ H.text <|
                    if indexBeingEdited < 0 then
                        "Add"

                    else
                        "Save"
                ]
            ]
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
                renderClock ( i, state ) model.zone
            )
        |> H.section [ HA.style "display" "flex", HA.style "user-select" "none", HA.style "flex-wrap" "wrap" ]


renderClock : ( Int, ClockState ) -> Time.Zone -> Html Msg
renderClock ( index, { time, isMoving, setting } ) zone =
    let
        { logo, frequency } =
            setting
    in
    H.div
        [ HA.style "padding" "10px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ clock logo frequency zone time isMoving
        , H.aside
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "justify-content" "space-between"
            , HA.style "padding" "10px 10px 0"
            , HA.style "font-size" "8px"
            , HA.style "font-family" "sans-serif"
            , HA.style "text-transform" "uppercase"
            ]
            [ H.a
                [ onClick << ToggleClock index <| not isMoving
                , HA.style "cursor" "pointer"
                ]
                [ text <|
                    if isMoving then
                        "Pause"

                    else
                        "Resume"
                ]
            , H.a
                [ onClick <| EditClock index
                , HA.style "cursor" "pointer"
                , HA.style "color" "green"
                ]
                [ text "Update" ]
            , H.a
                [ onClick <| DeleteClock index
                , HA.style "cursor" "pointer"
                , HA.style "color" "red"
                ]
                [ text "Delete" ]
            ]
        ]
