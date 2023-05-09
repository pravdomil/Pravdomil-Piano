module Piano.Utils.Theme exposing (..)

import Element.PravdomilUi.Theme
import Element.PravdomilUi.Theme.Light


theme : Element.PravdomilUi.Theme.Theme msg {}
theme =
    Element.PravdomilUi.Theme.Light.theme style


style : Element.PravdomilUi.Theme.Style {}
style =
    Element.PravdomilUi.Theme.Light.style
