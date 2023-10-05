module Logo exposing
    ( Logo
    , accutron
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


type alias Logo =
    { aspect : ( Float, Float )
    , width : Float
    , url : String
    , name : String
    }


nothing : Logo
nothing =
    Logo ( 0, 0 ) 0 "" ""


citizen : Logo
citizen =
    Logo ( 300.00001, 56.390969 ) 50 "/assets/images/citizen.svg" "Citizen"


seiko : Logo
seiko =
    Logo ( 500, 98.4 ) 48 "/assets/images/seiko.svg" "Seiko"


rolex : Logo
rolex =
    Logo ( 373, 207 ) 42 "/assets/images/rolex.svg" "Rolex"


breitling : Logo
breitling =
    Logo ( 1454, 674 ) 60 "/assets/images/breitling.png" "Breitling"


bulova : Logo
bulova =
    Logo ( 451, 205 ) 50 "/assets/images/bulova.svg" "Bulova"


accutron : Logo
accutron =
    Logo ( 940.02, 142.61 ) 64 "/assets/images/accutron.svg" "Accutron"


eterna : Logo
eterna =
    Logo ( 252.00002, 140.89999 ) 50 "/assets/images/eterna.svg" "Eterna"


longines : Logo
longines =
    Logo ( 119, 30 ) 60 "/assets/images/longines.svg" "Longines"


tissot : Logo
tissot =
    Logo ( 393.0, 143.77 ) 54 "/assets/images/tissot.svg" "Tissot"


titoni : Logo
titoni =
    Logo ( 178, 106 ) 54 "/assets/images/Titoni_Switzerland_Logo.svg" "Titoni"


rado : Logo
rado =
    Logo ( 1024, 324 ) 54 "/assets/images/rado.svg" "Rado"


piaget : Logo
piaget =
    Logo ( 1024, 414 ) 40 "/assets/images/piaget.svg" "Piaget"


omega : Logo
omega =
    Logo ( 254, 135 ) 50 "/assets/images/omega.svg" "Omega"


hublot : Logo
hublot =
    Logo ( 69.273984, 37.043056 ) 50 "/assets/images/hublot.svg" "Hublot"


oris : Logo
oris =
    Logo ( 856.65332, 340.16 ) 48 "/assets/images/oris.svg" "Oris"


iwc : Logo
iwc =
    Logo ( 133.18, 59.58 ) 54 "/assets/images/iwc.svg" "IWC"


zenith : Logo
zenith =
    Logo ( 409, 144 ) 60 "/assets/images/zenith.svg" "Zenith"


vacheronConstantin : Logo
vacheronConstantin =
    Logo ( 900, 233 ) 90 "/assets/images/vacheron.svg" "Vacheron Constantin"


union : Logo
union =
    Logo ( 1024, 356 ) 60 "/assets/images/union.svg" "Union"


frederiqueConstant : Logo
frederiqueConstant =
    Logo ( 466.22, 171.79 ) 96 "/assets/images/frederique-constant.svg" "Frederique Constant"


girardPerregaux : Logo
girardPerregaux =
    Logo ( 2000, 455 ) 80 "/assets/images/girard-perregaux.png" "Girard Perregaux"


orient : Logo
orient =
    Logo ( 158.85, 82.42 ) 40 "/assets/images/orient.svg" "Orient"


patekPhilippe : Logo
patekPhilippe =
    Logo ( 159.50522, 87.271477 ) 72 "/assets/images/Patek_Philippe_SA_logo.svg" "Patek Philippe"
