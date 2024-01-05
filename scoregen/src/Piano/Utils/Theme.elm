module Piano.Utils.Theme exposing (..)

import Element exposing (..)
import Element.Border
import Element.Font
import Element.Region


type alias EdgesXY =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }



--


white =
    rgb 1 1 1


black =
    rgb 0 0 0


lightGray =
    rgb 0.9 0.9 0.9


darkGray =
    rgb 0.8 0.8 0.8


blue =
    rgb 0 0 1



--


page a =
    Element.Font.family
        [ Element.Font.typeface "system-ui"
        , Element.Font.typeface "-apple-system"
        , Element.Font.typeface "Segoe UI"
        , Element.Font.typeface "Roboto"
        , Element.Font.typeface "Helvetica Neue"
        , Element.Font.typeface "Arial"
        , Element.Font.typeface "Noto Sans"
        , Element.Font.typeface "Liberation Sans"
        , Element.Font.sansSerif
        , Element.Font.typeface "Apple Color Emoji"
        , Element.Font.typeface "Segoe UI Emoji"
        , Element.Font.typeface "Segoe UI Symbol"
        , Element.Font.typeface "Noto Color Emoji"
        ]
        :: Element.Font.size 16
        :: a


heading1 a =
    Element.Region.heading 1 :: Element.Font.size 32 :: a


heading2 a =
    Element.Region.heading 2 :: Element.Font.size 28 :: a


heading3 a =
    Element.Region.heading 3 :: Element.Font.size 24 :: a


link_ a =
    Element.Font.color blue
        :: Element.Border.rounded 4
        :: focused
            [ Element.Border.shadow
                { color = fromRgb ((\x -> { x | alpha = 0.4 }) (toRgb blue))
                , offset = ( 0, 0 )
                , blur = 0
                , size = 4
                }
            ]
        :: a
