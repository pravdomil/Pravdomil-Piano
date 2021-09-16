module Piano exposing (..)

import Browser
import Browser.Navigation
import Browser.QueryRouter
import Element.PravdomilUi exposing (..)
import File
import File.Select
import Json.Decode
import Midi
import Midi.Decode
import Piano.Model
import Piano.Translation
import Piano.UserInterface exposing (..)
import Task
import Url


main : Program Json.Decode.Value Piano.Model.Model Piano.Model.Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = Piano.Model.UrlRequested
        , onUrlChange = Piano.Model.UrlChanged
        }



--


init : Json.Decode.Value -> Url.Url -> Browser.Navigation.Key -> ( Piano.Model.Model, Cmd Piano.Model.Msg )
init _ url key =
    let
        ( router, routerCmd ) =
            Browser.QueryRouter.init Piano.Model.pageFromUrl url key Piano.Model.UrlChanged
    in
    ( { router = router
      , file = Err Piano.Model.NotLoaded
      }
    , routerCmd
    )



--


update : Piano.Model.Msg -> Piano.Model.Model -> ( Piano.Model.Model, Cmd Piano.Model.Msg )
update msg model =
    case msg of
        Piano.Model.UrlRequested a ->
            ( model
            , Browser.QueryRouter.urlRequested a model.router
            )

        Piano.Model.UrlChanged a ->
            ( { model | router = Browser.QueryRouter.urlChanged Piano.Model.pageFromUrl a model.router }
            , Cmd.none
            )

        Piano.Model.SelectFile ->
            ( model
            , File.Select.file [ "audio/midi" ] Piano.Model.FileSelected
            )

        Piano.Model.FileSelected b ->
            ( model
            , File.toBytes b |> Task.perform (Piano.Model.FileLoaded b)
            )

        Piano.Model.FileLoaded b c ->
            ( { model
                | file =
                    c
                        |> Midi.Decode.file
                        |> Result.fromMaybe Piano.Model.DecodeError
                        |> Result.map (Tuple.pair b)
              }
            , Cmd.none
            )



--


subscriptions : Piano.Model.Model -> Sub Piano.Model.Msg
subscriptions _ =
    Sub.none



--


view : Piano.Model.Model -> Browser.Document Piano.Model.Msg
view model =
    { title = Piano.Translation.title
    , body =
        [ Element.PravdomilUi.layout theme [] (viewBody model)
        ]
    }


viewBody : Piano.Model.Model -> Element Piano.Model.Msg
viewBody model =
    column [ padding 16, spacing 16 ]
        [ button theme
            []
            { label = text "Select MIDI File"
            , onPress = Just Piano.Model.SelectFile
            }
        , case model.file of
            Ok ( b, c ) ->
                column [ spacing 8 ]
                    ([ paragraph theme
                        []
                        [ text ("Name: " ++ File.name b)
                        ]
                     , paragraph theme
                        []
                        [ text
                            ("Format: "
                                ++ (case c.format of
                                        Midi.Simultaneous ->
                                            "Simultaneous"

                                        Midi.Independent ->
                                            "Independent"
                                   )
                            )
                        ]
                     , paragraph theme
                        []
                        [ text ("Tempo: " ++ (\(Midi.TicksPerBeat v) -> String.fromInt v) c.tempo)
                        ]
                     ]
                        ++ (c.tracks |> (\( v1, v2 ) -> v1 :: v2) |> List.map viewTrack)
                    )

            Err b ->
                case b of
                    Piano.Model.NotLoaded ->
                        none

                    Piano.Model.DecodeError ->
                        paragraph theme
                            []
                            [ text "Cannot read MIDI file."
                            ]
        ]


viewTrack : Midi.Track -> Element msg
viewTrack a =
    text (Debug.toString a)
