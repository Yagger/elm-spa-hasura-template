module Components.MobileMenu exposing (button, view)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Br
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as In
import Palette exposing (palette)
import Responsive exposing (Responsive, fontSize, frw, frwf, pad)
import Spa.Generated.Route as Route


button : Responsive -> msg -> Element msg
button r onClick =
    In.button
        [ alignRight ]
        { onPress = Just onClick
        , label =
            column
                [ width <| px <| frw r 50, spacing <| frw r 16 ]
                [ el [ width fill, height <| px 3, Bg.color palette.dark ] none
                , el [ width fill, height <| px 3, Bg.color palette.dark ] none
                , el [ width fill, height <| px 3, Bg.color palette.dark ] none
                ]
        }


spacer : Attribute msg
spacer =
    Br.widthEach { top = 0, right = 0, bottom = 1, left = 0 }


view : Responsive -> msg -> Element msg
view r onClose =
    column
        [ width fill
        , height fill
        , Bg.color palette.dark
        , pad r 30 40 50 40
        , Font.color palette.white
        ]
        [ row
            [ width fill, spacer, pad r 0 0 30 0 ]
            [ In.button [] { onPress = Just onClose, label = el [ fontSize r 66 ] <| text "✕" }
            , if r.device.class == Tablet then
                image [ width <| minimum 150 <| px <| frw r 150, pad r 5 0 0 40 ]
                    { description = "logo", src = "/img/logo.png" }

              else
                none
            , el [ fontSize r 36, alignRight, onClick onClose ] <|
                link []
                    { url = "/", label = text "Home" }
            ]
        , el [ width fill, spacer, pad r 40 0 40 0, fontSize r 36, onClick onClose ] <|
            link []
                { url = Route.toString Route.Login, label = text "Login" }
        , el [ width fill, spacer, pad r 40 0 40 0, fontSize r 36, onClick onClose ] <|
            link []
                { url = Route.toString Route.Register, label = text "Register" }
        ]
