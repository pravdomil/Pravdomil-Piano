port module WebMidi exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task


init : Task.Task JavaScript.Error ()
init =
    JavaScript.run
        """
navigator.requestMIDIAccess().then(a => {
  a.inputs.forEach(b => { b.onmidimessage = c => { scope.ports.webMidi.send(c.data.buffer) } })
})
"""
        Json.Encode.null
        (Json.Decode.succeed ())


port webMidi : (Json.Decode.Value -> msg) -> Sub msg
