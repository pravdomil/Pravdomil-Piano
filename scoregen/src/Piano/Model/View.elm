module Piano.Model.View exposing (..)

import Browser
import Dict.Any
import Element exposing (..)
import Element.Background
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
        [ layout [ width fill, Element.Font.size 16, Element.Background.color deepBlack, Element.Font.color white ] (viewBody model)
        , Html.node "style" [] [ Html.text "body{background-color:rgb(38,38,38)}" ]
        ]


viewBody : Piano.Model.Model -> Element Piano.Msg.Msg
viewBody model =
    column [ width fill, spacing 8 ]
        [ el [] none
        , viewHeader model
        , case model.file of
            Ok b ->
                Piano.File.View.viewFile model.activeNotes b

            Err _ ->
                none
        , el [ height (px 80) ] none
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
        , Element.Input.button
            (interactive [ height fill ])
            { label = text "Select MIDI File"
            , onPress = Just Piano.Msg.FileSelectRequested
            }
        ]
