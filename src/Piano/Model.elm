module Piano.Model exposing (..)

import Browser
import Browser.QueryRouter
import Bytes
import Dict
import Dict.Any
import File
import Midi
import Url
import Url.QueryString


type alias Model =
    { router : Browser.QueryRouter.QueryRouter Page
    , file : Result FileError File
    }


type alias File =
    { file : File.File
    , midi : Midi.File
    , disabledTracks : Dict.Any.Dict TrackNumber ()
    }


type FileError
    = NotLoaded
    | DecodeError



--


type alias Page =
    { query : Dict.Dict String String
    }


pageFromUrl : Url.Url -> Page
pageFromUrl a =
    { query = a.query |> Maybe.withDefault "" |> Url.QueryString.fromString
    }


pageToUrl : Page -> String
pageToUrl a =
    "?" ++ Url.QueryString.toString a.query



--


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectFile
    | FileSelected File.File
    | FileLoaded File.File Bytes.Bytes
    | ToogleTrack TrackNumber Bool



--


type TrackNumber
    = TrackNumber Int


trackNumberToInt : TrackNumber -> Int
trackNumberToInt (TrackNumber a) =
    a
