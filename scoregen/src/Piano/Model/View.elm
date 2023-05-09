module Piano.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import File
import Piano.File.View
import Piano.Model
import Piano.Msg
import Piano.TrackNumber
import Piano.Utils.Theme exposing (..)


view : Piano.Model.Model -> Browser.Document Piano.Msg.Msg
view model =
    { title = "Pravdomil Piano"
    , body =
        [ Element.PravdomilUi.layout theme [ width fill, height (px 800) ] (viewBody model)
        ]
    }


viewBody : Piano.Model.Model -> Element Piano.Msg.Msg
viewBody model =
    column [ width fill, height fill ]
        [ el [ height (px 8) ] none
        , viewHeader model
        , case model.file of
            Ok b ->
                Piano.File.View.viewFile b

            Err _ ->
                none
        ]


viewHeader : Piano.Model.Model -> Element Piano.Msg.Msg
viewHeader model =
    row [ spacing 8, centerX ]
        [ case model.file of
            Ok b ->
                text (File.name b.file)

            Err b ->
                case b of
                    Piano.Model.NotLoaded ->
                        text "Pravdomil Piano Score Generator"

                    Piano.Model.DecodeError ->
                        text "Cannot read MIDI file."
        , case model.file of
            Ok b ->
                row [ spacing 8 ]
                    (b.midi.tracks
                        |> (\( x, x2 ) -> x :: x2)
                        |> List.indexedMap
                            (\i _ ->
                                let
                                    number : Piano.TrackNumber.TrackNumber
                                    number =
                                        Piano.TrackNumber.fromInt i
                                in
                                inputCheckbox
                                    theme
                                    []
                                    { icon = inputCheckboxIcon theme
                                    , label = text (String.fromInt (Piano.TrackNumber.toInt number + 1))
                                    , checked = b.disabledTracks |> Dict.Any.member Piano.TrackNumber.toInt number |> not
                                    , onChange = Piano.Msg.TrackToggleRequested number
                                    }
                            )
                    )

            Err _ ->
                none
        , button theme
            []
            { label = text "Select MIDI File"
            , active = False
            , onPress = Just Piano.Msg.FileSelectRequested
            }
        ]
