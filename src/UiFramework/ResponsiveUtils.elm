module UiFramework.ResponsiveUtils exposing (UiElement, classifyDevice, wrapContent)

import Element exposing (Device, DeviceClass(..), Orientation(..), el, fill, fillPortion, none, row, width)
import UiFramework.Internal as Internal


type alias UiElement context msg =
    Internal.WithContext (Internal.UiContextual context) msg



-- Use Bootstrap responsive breakpoints: https://getbootstrap.com/docs/4.3/layout/overview/#responsive-breakpoints


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    { class =
        if window.width < 500 then
            Phone

        else if window.width < 850 then
            Tablet

        else if window.width < 1200 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }


wrapContent : UiElement context msg -> UiElement context msg
wrapContent content =
    Internal.fromElement
        (\context ->
            let
                ( left, middle, right ) =
                    case context.device.class of
                        BigDesktop ->
                            ( 1, 4, 1 )

                        Desktop ->
                            ( 1, 4, 1 )

                        Tablet ->
                            case context.device.orientation of
                                Portrait ->
                                    ( 1, 8, 1 )

                                Landscape ->
                                    ( 1, 8, 1 )

                        Phone ->
                            ( 0, 1, 0 )
            in
            -- basically squish the content between two Element.none
            row
                [ width fill ]
                [ el [ width <| fillPortion left ] none
                , el [ width <| fillPortion middle ] (Internal.toElement context content)
                , el [ width <| fillPortion right ] none
                ]
        )
