module Logo exposing
    ( breitling
    , bulova
    , citizen
    , eterna
    , frederiqueConstant
    , girardPerregaux
    , hublot
    , iwc
    , logines
    , omega
    , orient
    , oris
    , piaget
    , rado
    , rolex
    , seiko
    , tissot
    , titoni
    , union
    , vacheronConstantin
    , zenith
    )

import Svg exposing (..)
import Svg.Attributes exposing (..)


citizen : Svg msg
citizen =
    clockLogo ( 300.00001, 56.390969 ) 50 "/assets/images/citizen.svg"


seiko : Svg msg
seiko =
    clockLogo ( 500, 98.4 ) 48 "/assets/images/seiko.svg"


rolex : Svg msg
rolex =
    clockLogo ( 743.5, 371.75 ) 50 "/assets/images/rolex.svg"


breitling : Svg msg
breitling =
    clockLogo ( 1454, 674 ) 60 "/assets/images/breitling.png"


bulova : Svg msg
bulova =
    clockLogo ( 210, 40 ) 50 "/assets/images/bulova.svg"


eterna : Svg msg
eterna =
    clockLogo ( 252.00002, 140.89999 ) 50 "/assets/images/eterna.svg"


logines : Svg msg
logines =
    clockLogo ( 5000, 1241 ) 72 "/assets/images/logines.png"


tissot : Svg msg
tissot =
    clockLogo ( 393.0, 143.77 ) 50 "/assets/images/tissot.svg"


titoni : Svg msg
titoni =
    clockLogo ( 739, 441 ) 40 "/assets/images/titoni.png"


rado : Svg msg
rado =
    clockLogo ( 1024, 324 ) 40 "/assets/images/rado.svg"


piaget : Svg msg
piaget =
    clockLogo ( 1024, 414 ) 40 "/assets/images/piaget.svg"


omega : Svg msg
omega =
    clockLogo ( 254, 135 ) 40 "/assets/images/omega.svg"


hublot : Svg msg
hublot =
    clockLogo ( 69.273984, 37.043056 ) 40 "/assets/images/hublot.svg"


oris : Svg msg
oris =
    clockLogo ( 3126, 1701 ) 40 "/assets/images/oris.png"


iwc : Svg msg
iwc =
    clockLogo ( 133.18, 59.58 ) 50 "/assets/images/iwc.svg"


zenith : Svg msg
zenith =
    clockLogo ( 133.18, 59.58 ) 44 "/assets/images/zenith.svg"


vacheronConstantin : Svg msg
vacheronConstantin =
    clockLogo ( 900, 233 ) 90 "/assets/images/vacheron.svg"


union : Svg msg
union =
    clockLogo ( 1024, 356 ) 40 "/assets/images/union.svg"


frederiqueConstant : Svg msg
frederiqueConstant =
    clockLogo ( 5000, 1848 ) 80 "/assets/images/frederique-constant.png"


girardPerregaux : Svg msg
girardPerregaux =
    clockLogo ( 2000, 455 ) 80 "/assets/images/girard-perregaux.png"


orient : Svg msg
orient =
    clockLogo ( 158.85, 82.42 ) 40 "/assets/images/orient.svg"


clockLogo : ( Float, Float ) -> Float -> String -> Svg msg
clockLogo ( aspectWidth, aspectHeight ) width url =
    let
        ratio =
            aspectHeight / aspectWidth

        height =
            ratio * width

        x =
            (200 - width) / 2

        y =
            (100 - height) / goldenRatio
    in
    Svg.node "image"
        [ Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| String.fromFloat width
        , Svg.Attributes.height <| String.fromFloat height
        , Svg.Attributes.xlinkHref url
        ]
        []


goldenRatio : Float
goldenRatio =
    (1 + sqrt 5) / 2
