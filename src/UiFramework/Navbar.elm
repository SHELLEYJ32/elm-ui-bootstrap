module UiFramework.Navbar exposing
    ( MenuItem(..)
    , Navbar
    , NavbarState
    , customItem
    , default
    , linkItem
    , view
    , withBackground
    , withBackgroundColor
    , withBrand
    , withExtraAttrs
    , withMenuIcon
    , withMenuItems
    , withMenuTitle
    )

import Element
    exposing
        ( Attribute
        , Color
        , Device
        , DeviceClass(..)
        , Element
        , Orientation(..)
        , alignLeft
        , alignRight
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , none
        , padding
        , paddingXY
        , pointer
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html.Events
import Json.Decode as Json
import UiFramework.Configuration exposing (ThemeConfig)
import UiFramework.Dropdown as Dropdown
import UiFramework.Icon as Icon
import UiFramework.Internal as Internal
import UiFramework.Types exposing (Role(..), Size(..))


type alias NavbarState state =
    { toggleMenuState : Bool
    , dropdownState : state
    }


type alias Context context =
    { context
        | device : Device
        , themeConfig : ThemeConfig
        , parentRole : Maybe Role
    }


type alias UiElement context msg =
    Internal.WithContext (Context context) msg


type Navbar context state msg
    = Navbar (NavbarOptions context state msg)


type alias NavbarOptions context state msg =
    { toggleMenuMsg : msg
    , brand : Maybe (Element msg)
    , backgroundColor : BackgroundColor
    , items : List (MenuItem context state msg)
    , attributes : List (Attribute msg)
    }


type BackgroundColor
    = Roled Role
    | Custom Color


type MenuItem context state msg
    = LinkItem (LinkItemOptions msg)
    | DropdownItem (Dropdown.Dropdown context state msg)
    | CustomItem String


type alias LinkItemOptions msg =
    { triggerMsg : msg
    , icon : Maybe Icon.Icon
    , title : String
    }


withBrand : Element msg -> Navbar context state msg -> Navbar context state msg
withBrand brand (Navbar options) =
    Navbar { options | brand = Just brand }


withBackground : Role -> Navbar context state msg -> Navbar context state msg
withBackground role (Navbar options) =
    Navbar { options | backgroundColor = Roled role }


withBackgroundColor : Color -> Navbar context state msg -> Navbar context state msg
withBackgroundColor color (Navbar options) =
    Navbar { options | backgroundColor = Custom color }


withMenuItems : List (MenuItem context state msg) -> Navbar context state msg -> Navbar context state msg
withMenuItems items (Navbar options) =
    Navbar { options | items = items }


withExtraAttrs : List (Attribute msg) -> Navbar context state msg -> Navbar context state msg
withExtraAttrs attributes (Navbar options) =
    Navbar { options | attributes = attributes }


default : msg -> Navbar context state msg
default msg =
    Navbar
        { toggleMenuMsg = msg
        , brand = Nothing
        , backgroundColor = Roled Light
        , items = []
        , attributes = []
        }


linkItem : msg -> MenuItem context state msg
linkItem msg =
    LinkItem
        { triggerMsg = msg
        , icon = Nothing
        , title = ""
        }


customItem : String -> MenuItem context state msg
customItem title =
    CustomItem title


withMenuIcon : Icon.Icon -> MenuItem context state msg -> MenuItem context state msg
withMenuIcon icon item =
    case item of
        LinkItem options ->
            LinkItem { options | icon = Just icon }

        DropdownItem dropdown ->
            dropdown
                |> Dropdown.withIcon icon
                |> DropdownItem

        CustomItem title ->
            item


withMenuTitle : String -> MenuItem context state msg -> MenuItem context state msg
withMenuTitle title item =
    case item of
        LinkItem options ->
            LinkItem { options | title = title }

        DropdownItem dropdown ->
            dropdown
                |> Dropdown.withTitle title
                |> DropdownItem

        CustomItem _ ->
            CustomItem title



-- Render Navbar


view : NavbarState state -> Navbar context state msg -> UiElement context msg
view { toggleMenuState, dropdownState } (Navbar options) =
    Internal.flatMap
        (\context ->
            let
                navbarConfig =
                    context.themeConfig.navbarConfig

                backgroundColor =
                    case options.backgroundColor of
                        Roled role ->
                            context.themeConfig.themeColor role

                        Custom color ->
                            color

                fontColor =
                    context.themeConfig.fontColor backgroundColor

                headerAttrs =
                    [ width fill
                    , paddingXY navbarConfig.paddingX navbarConfig.paddingY
                    , Background.color backgroundColor
                    , Font.color fontColor
                    ]

                brand attrs =
                    Internal.fromElement
                        (\_ ->
                            el attrs
                                (options.brand |> Maybe.withDefault none)
                        )

                navButton toggleMenuMsg =
                    Internal.fromElement
                        (\_ ->
                            el
                                [ onClick toggleMenuMsg
                                , alignLeft
                                , paddingXY navbarConfig.togglerPaddingX navbarConfig.togglerPaddingY
                                , Border.color fontColor
                                , Border.solid
                                , Border.width 1
                                , Border.rounded navbarConfig.togglerBorderRadius
                                , pointer
                                ]
                            <|
                                column
                                    [ spacing 4, padding 5 ]
                                    [ iconBar, iconBar, iconBar ]
                        )

                iconBar =
                    el
                        [ width <| px 20
                        , height <| px 2
                        , Background.color fontColor
                        , Border.rounded 1
                        ]
                        none
            in
            if collapseNavbar context.device then
                Internal.uiColumn headerAttrs <|
                    [ Internal.uiRow [ width fill ]
                        [ navButton options.toggleMenuMsg
                        , brand [ alignRight ]
                        ]
                    ]
                        ++ (if toggleMenuState then
                                [ viewCollapsedMenuList dropdownState options.items ]

                            else
                                []
                           )

            else
                Internal.uiRow headerAttrs [ viewMenubarList dropdownState options.items, brand [ alignRight ] ]
        )


collapseNavbar : Device -> Bool
collapseNavbar device =
    case device.class of
        Phone ->
            True

        Tablet ->
            if device.orientation == Portrait then
                True

            else
                False

        _ ->
            False


viewCollapsedMenuList : state -> List (MenuItem context state msg) -> UiElement context msg
viewCollapsedMenuList dropdownState items =
    Internal.fromElement
        (\context ->
            column
                [ Region.navigation
                , width fill
                , alignLeft
                , Font.alignLeft
                , paddingXY 0 10
                , spacing 5
                ]
            <|
                List.map (viewMenuItem dropdownState >> Internal.toElement context) items
        )


viewMenubarList : state -> List (MenuItem context state msg) -> UiElement context msg
viewMenubarList dropdownState items =
    Internal.fromElement
        (\context ->
            row
                [ Region.navigation
                , alignLeft
                , Font.center
                ]
            <|
                List.map (viewMenuItem dropdownState >> Internal.toElement context) items
        )


viewMenuItem : state -> MenuItem context state msg -> UiElement context msg
viewMenuItem dropdownState item =
    case item of
        LinkItem options ->
            viewLinkItem options

        DropdownItem dropdown ->
            Dropdown.view dropdownState dropdown

        CustomItem title ->
            Internal.uiText (\context -> title)


viewLinkItem : LinkItemOptions msg -> UiElement context msg
viewLinkItem options =
    Internal.fromElement
        (\context ->
            el
                [ onClick options.triggerMsg
                , paddingXY
                    context.themeConfig.navConfig.linkPaddingX
                    context.themeConfig.navConfig.linkPaddingY
                , width fill
                , pointer
                ]
                (case options.icon of
                    Nothing ->
                        text options.title

                    Just icon ->
                        row [ spacing 5 ] [ el [] <| Icon.view icon, el [] (text options.title) ]
                )
        )


onClick : msg -> Attribute msg
onClick message =
    Html.Events.custom
        "click"
        (Json.succeed
            { message = message
            , stopPropagation = True
            , preventDefault = False
            }
        )
        |> htmlAttribute
