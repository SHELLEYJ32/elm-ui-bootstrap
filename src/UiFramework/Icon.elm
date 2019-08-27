module UiFramework.Icon exposing (Icon, view, viewWithAttribute)

import Element exposing (Attribute, Element, el, html)
import FontAwesome.Icon


type alias Icon =
    FontAwesome.Icon.Icon


view : Icon -> Element msg
view icon =
    FontAwesome.Icon.viewIcon icon
        |> html


viewWithAttribute : List (Attribute msg) -> Icon -> Element msg
viewWithAttribute attr icon =
    FontAwesome.Icon.viewIcon icon
        |> html
        |> el attr
