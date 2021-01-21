module Responsive exposing (Responsive, fontSize, frw, frwf, make, pad)

import Element exposing (Attr, Attribute, Device, DeviceClass(..), classifyDevice, paddingEach)
import Element.Font as Font


maxWidth =
    1920


minWidth =
    0


nominalWidth : Responsive -> Int
nominalWidth r =
    case r.device.class of
        Phone ->
            480

        Tablet ->
            950

        Desktop ->
            1920

        BigDesktop ->
            1920


type alias Responsive =
    { screenWidth : Int
    , screenHeight : Int
    , width : Int
    , height : Int
    , device : Device
    }


fontSize : Responsive -> Int -> Attr decorative msg
fontSize r x =
    (toFloat x / toFloat (nominalWidth r)) * toFloat r.width |> round |> Font.size



-- Fraction width


frw : Responsive -> Int -> Int
frw r px =
    floor (toFloat r.width * (toFloat px / toFloat (nominalWidth r)))


frwf : Responsive -> Int -> Float
frwf r px =
    toFloat r.width * (toFloat px / toFloat (nominalWidth r))


pad : Responsive -> Int -> Int -> Int -> Int -> Attribute msg
pad r top right bottom left =
    paddingEach { top = frw r top, right = frw r right, bottom = frw r bottom, left = frw r left }


make : Int -> Int -> Responsive
make screenWidth screenHeight =
    let
        width =
            if screenWidth < minWidth then
                minWidth

            else if screenWidth > maxWidth then
                maxWidth

            else
                screenWidth
    in
    { screenWidth = screenWidth
    , screenHeight = screenHeight
    , width = width
    , height = screenHeight
    , device = classifyDevice { width = screenWidth, height = screenHeight }
    }
