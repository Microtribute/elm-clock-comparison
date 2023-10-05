module Logo exposing
    ( accutron
    , breitling
    , bulova
    , citizen
    , eterna
    , frederiqueConstant
    , girardPerregaux
    , hublot
    , iwc
    , longines
    , nothing
    , omega
    , orient
    , oris
    , patekPhilippe
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


nothing : Svg msg
nothing =
    Svg.text ""


citizen : Svg msg
citizen =
    clockLogo ( 300.00001, 56.390969 ) 50 "/assets/images/citizen.svg"


seiko : Svg msg
seiko =
    clockLogo ( 500, 98.4 ) 48 "/assets/images/seiko.svg"


rolex : Svg msg
rolex =
    clockLogo ( 373, 207 ) 42 "/assets/images/rolex.svg"


breitling : Svg msg
breitling =
    clockLogo ( 1454, 674 ) 60 "/assets/images/breitling.png"


bulova : Svg msg
bulova =
    clockLogo ( 451, 205 ) 50 "/assets/images/bulova.svg"


accutron : Svg msg
accutron =
    clockLogo ( 940.02, 142.61 ) 64 "/assets/images/accutron.svg"


eterna : Svg msg
eterna =
    clockLogo ( 252.00002, 140.89999 ) 50 "/assets/images/eterna.svg"


longines : Svg msg
longines =
    clockLogo ( 119, 30 ) 60 "/assets/images/longines.svg"


tissot : Svg msg
tissot =
    clockLogo ( 393.0, 143.77 ) 54 "/assets/images/tissot.svg"


titoni : Svg msg
titoni =
    clockLogo ( 178, 106 ) 54 "/assets/images/Titoni_Switzerland_Logo.svg"


rado : Svg msg
rado =
    clockLogo ( 1024, 324 ) 40 "/assets/images/rado.svg"


piaget : Svg msg
piaget =
    clockLogo ( 1024, 414 ) 40 "/assets/images/piaget.svg"


omega : Svg msg
omega =
    clockLogo ( 254, 135 ) 50 "/assets/images/omega.svg"


hublot : Svg msg
hublot =
    clockLogo ( 69.273984, 37.043056 ) 50 "/assets/images/hublot.svg"


oris : Svg msg
oris =
    clockLogo ( 856.65332, 340.16 ) 48 "/assets/images/oris.svg"


iwc : Svg msg
iwc =
    clockLogo ( 133.18, 59.58 ) 54 "/assets/images/iwc.svg"


zenith : Svg msg
zenith =
    clockLogo ( 409, 144 ) 60 "/assets/images/zenith.svg"


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


patekPhilippe : Svg msg
patekPhilippe =
    clockLogo ( 159.50522, 87.271477 ) 72 "/assets/images/Patek_Philippe_SA_logo.svg"


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
