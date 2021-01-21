module Palette exposing (palette)

import Element exposing (rgb255, rgba255)


palette =
    -- colors
    { dark = rgb255 60 60 60
    , fontGrey = rgb255 116 118 120
    , fontLightGrey = rgb255 203 204 205
    , white = rgb255 255 255 255
    , shadow = rgba255 0 0 0 0.25
    , green = rgb255 8 167 66
    , activeGreen = rgb255 13 146 62
    , transparentGreen = rgb255 97 199 134
    , red = rgb255 214 67 54
    , grey = rgb255 65 67 70
    , almostWhite = rgb255 247 247 247
    , bgGreyTransparent = rgba255 247 247 247 0.5

    -- color classes
    , bgGrey238 = rgb255 238 238 238
    , bgGrey251 = rgb255 251 251 251
    , bgGrey196 = rgb255 196 196 196
    , bgGrey123 = rgb255 123 123 123
    }
