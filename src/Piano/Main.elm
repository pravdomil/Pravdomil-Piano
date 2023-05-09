module Piano.Main exposing (..)

import Browser
import Browser.Navigation
import Browser.QueryRouter
import Dict.Any
import Element.PravdomilUi exposing (..)
import File
import File.Select
import Json.Decode
import Midi.Decode
import Piano.Model
import Piano.Score
import Piano.Utils.Theme exposing (..)
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
                        |> Result.map (\v -> Piano.Model.File b v Dict.Any.empty)
              }
            , Cmd.none
            )

        Piano.Model.ToogleTrack b c ->
            ( { model
                | file =
                    model.file
                        |> Result.map
                            (\v ->
                                { v
                                    | disabledTracks =
                                        if not c then
                                            Dict.Any.insert Piano.Model.trackNumberToInt b () v.disabledTracks

                                        else
                                            Dict.Any.remove Piano.Model.trackNumberToInt b v.disabledTracks
                                }
                            )
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
    { title = "Pravdomil Piano"
    , body =
        [ Element.PravdomilUi.layout theme [ width fill, height (px 800) ] (viewBody model)
        ]
    }


viewBody : Piano.Model.Model -> Element Piano.Model.Msg
viewBody model =
    column [ width fill, height fill, spacing 16 ]
        [ viewHeader model
        , case model.file of
            Ok b ->
                Piano.Score.viewFile b

            Err _ ->
                none
        ]


viewHeader : Piano.Model.Model -> Element Piano.Model.Msg
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
                            |> (\( v1, v2 ) -> v1 :: v2)
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
