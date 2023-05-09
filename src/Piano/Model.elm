module Piano.Model exposing (..)

import File


type alias Model =
    { file : Result FileError File.File
    }



--


type FileError
    = NotLoaded
    | DecodeError
