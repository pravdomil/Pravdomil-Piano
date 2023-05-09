module Piano.Main exposing (..)

import Browser
import Json.Decode
import Piano.Model
import Piano.Model.Update
import Piano.Model.View
import Piano.Msg


main : Program Json.Decode.Value Piano.Model.Model Piano.Msg.Msg
main =
    Browser.document
        { init = Piano.Model.Update.init
        , update = Piano.Model.Update.update
        , subscriptions = Piano.Model.Update.subscriptions
        , view = Piano.Model.View.view
        }
