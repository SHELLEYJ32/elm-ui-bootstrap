module UiFramework.Icon exposing (Icon, view, viewLink)

import Element exposing (Attribute, Element, el, html)
import FontAwesome.Icon


type alias Icon =
    FontAwesome.Icon.Icon


view : Icon -> Element msg
view icon =
    FontAwesome.Icon.viewIcon icon
        |> html


viewLink : Icon -> List (Attribute msg) -> Element msg
viewLink icon attr =
    FontAwesome.Icon.viewIcon icon
        |> html
        |> el attr
