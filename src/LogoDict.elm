module LogoDict exposing (get, options)

import Logo


logos : List Logo.Logo
logos =
    [ Logo.nothing
    , Logo.accutron
    , Logo.breitling
    , Logo.bulova
    , Logo.citizen
    , Logo.eterna
    , Logo.frederiqueConstant
    , Logo.girardPerregaux
    , Logo.hublot
    , Logo.iwc
    , Logo.longines
    , Logo.orient
    , Logo.omega
    , Logo.oris
    , Logo.patekPhilippe
    , Logo.piaget
    , Logo.rado
    , Logo.rolex
    , Logo.seiko
    , Logo.tissot
    , Logo.titoni
    , Logo.union
    , Logo.vacheronConstantin
    , Logo.zenith
    ]


get : String -> Logo.Logo
get key =
    logos
        |> List.filter (\{ name } -> name == key)
        |> List.head
        |> Maybe.withDefault Logo.nothing


options : List String
options =
    List.map .name logos
