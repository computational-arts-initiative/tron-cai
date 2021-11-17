module JetBrains.Palette exposing
    ( Palette, decode )


import Json.Decode as D

import Color exposing (Color)


type alias Palette = List Color


decodeColor : D.Decoder Color
decodeColor
    = D.map3
            Color.rgb
            (D.field "r" D.int |> D.map (\r -> toFloat r / 255))
            (D.field "g" D.int |> D.map (\g -> toFloat g / 255))
            (D.field "b" D.int |> D.map (\b -> toFloat b / 255))


decode : D.Decoder Palette
decode = D.list decodeColor
