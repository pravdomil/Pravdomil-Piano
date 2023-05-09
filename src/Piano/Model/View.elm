module Piano.Model.View exposing (..)

import Browser
import Dict.Any
import Element.PravdomilUi exposing (..)
import File
import Piano.File.View
import Piano.Model
import Piano.Msg
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
    column [ width fill, height fill, spacing 16 ]
        [ viewHeader model
        , case model.file of
            Ok b ->
                Piano.File.View.viewFile b

            Err _ ->
                none
        ]


viewHeader : Piano.Model.Model -> Element Piano.Msg.Msg
viewHeader model =
    column [ width fill, paddingXY 16 0, spacing 16 ]
        [ el [] none
        , row [ spacing 8, centerX ]
            [ case model.file of
                Ok b ->
                    text (File.name b.file)

                Err b ->
                    case b of
                        Piano.Model.NotLoaded ->
                            none

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
                                        number : Piano.Model.TrackNumber
                                        number =
                                            Piano.Model.TrackNumber i
                                    in
                                    inputCheckbox
                                        []
                                        { icon = inputDefaultCheckbox
                                        , label = labelRight theme [] (text (String.fromInt (Piano.Model.trackNumberToInt number + 1)))
                                        , checked = b.disabledTracks |> Dict.Any.member Piano.Model.trackNumberToInt number |> not
                                        , onChange = Piano.Model.ToogleTrack number
                                        }
                                )
                        )

                Err _ ->
                    none
            , linkWithOnPress theme
                []
                { label = text "Select MIDI File"
                , onPress = Just Piano.Model.SelectFile
                }
            ]
        ]
