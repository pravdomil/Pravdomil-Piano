module Piano.TrackNumber exposing (..)


type TrackNumber
    = TrackNumber Int


fromInt : Int -> TrackNumber
fromInt =
    TrackNumber


toInt : TrackNumber -> Int
toInt (TrackNumber a) =
    a
