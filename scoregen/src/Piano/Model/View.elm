module Piano.Model.View exposing (..)

import Browser
import Dict.Any
import Element exposing (..)
import Element.Font
import Element.Input
import File
import Html
import Piano.File.View
import Piano.Model
import Piano.Msg
import Piano.TrackNumber
import Piano.Utils.Theme exposing (..)


view : Piano.Model.Model -> Browser.Document Piano.Msg.Msg
view model =
    Browser.Document
        "Pravdomil Piano Score Generator"
        [ layout [ width fill, Element.Font.size 16 ] (viewBody model)
        , Html.node "style" [] [ Html.text "body{background-color:rgb(230,230,230)}" ]
        ]


viewBody : Piano.Model.Model -> Element Piano.Msg.Msg
viewBody model =
    column [ width fill ]
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
    row [ spacing 16, centerX, height (px 40) ]
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
                row [ height fill ]
                    (List.indexedMap
                        (\i _ ->
                            let
                                number : Piano.TrackNumber.TrackNumber
                                number =
                                    Piano.TrackNumber.fromInt i
                            in
                            Element.Input.checkbox
                                (clickable [ paddingXY 8 0, height fill ])
                                { icon = Element.Input.defaultCheckbox
                                , label = Element.Input.labelRight [ centerY ] (text (String.fromInt (Piano.TrackNumber.toInt number + 1)))
                                , checked = not (Dict.Any.member Piano.TrackNumber.toInt number b.disabledTracks)
                                , onChange = Piano.Msg.TrackToggleRequested number
                                }
                        )
                        ((\( x, x2 ) -> x :: x2) b.midi.tracks)
                    )

            Err _ ->
                none
        , Element.Input.button
            (clickable [ height fill ])
            { label = text "Select MIDI File"
            , onPress = Just Piano.Msg.FileSelectRequested
            }
        ]
