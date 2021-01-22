module Components.Popup exposing (..)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Br
import Element.Font as Font
import Element.Input as In
import Html.Attributes exposing (class)
import Palette exposing (palette)
import Responsive exposing (Responsive, fontSize, frw, pad)


type Popup
    = HttpError String
    | Closed


centerContainer : Responsive -> msg -> Element msg -> Element msg
centerContainer r onClose content =
    el
        [ htmlAttribute <| class "popup"
        , width fill
        , height fill
        , pad r 7 106 7 106
        , scrollbarY
        ]
    <|
        el
            [ width (fill |> maximum 600)
            , height (shrink |> minimum 100)
            , centerX
            , centerY
            , Bg.color palette.white
            , Br.color palette.grey
            , Br.width 4
            , padding <| frw r 10
            , inFront <|
                In.button
                    [ Font.bold
                    , fontSize r 20
                    , alignRight
                    , padding <| frw r 10
                    , Bg.color palette.grey
                    , Font.color palette.white
                    ]
                    { onPress = Just onClose, label = text "╳" }
            ]
        <|
            content


cornerContainer : Responsive -> msg -> Element msg -> Element msg
cornerContainer r onClose content =
    el
        [ htmlAttribute <| class "corner-popup"
        , width (fill |> maximum 600)
        , height (shrink |> minimum 100)
        , alignTop
        , alignRight
        , moveDown 2
        , moveLeft 2
        , Bg.color palette.white
        , Br.color palette.grey
        , Br.width 4
        , padding <| frw r 10
        , inFront <|
            In.button
                [ Font.bold
                , fontSize r 20
                , alignRight
                , padding <| frw r 10
                , Bg.color palette.grey
                , Font.color palette.white
                ]
                { onPress = Just onClose, label = text "╳" }
        ]
    <|
        content


view : Responsive -> { popup : Popup, onClose : msg } -> Element msg
view r options =
    case options.popup of
        HttpError msg ->
            cornerContainer r
                options.onClose
                (column [] [ text msg ])

        Closed ->
            none
