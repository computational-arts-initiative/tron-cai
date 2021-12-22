module JetBrains.Blend exposing
    ( Blend(..)
    , toString, fromString
    , toLabel
    , all, basicOnes, forCanvas, forCss
    , equal
    , default
    )


type Blend
    = Normal
    | SourceOver
    | SourceIn
    | SourceOut
    | SourceAtop
    | DestinationOver
    | DestinationIn
    | DestinationOut
    | DestinationAtop
    | Lighter
    | Copy
    | Xor
    | Darken
    | Lighten
    | Multiply
    | Screen
    | Overlay
    | ColorDodge
    | ColorBurn
    | HardLight
    | SoftLight
    | Difference
    | Exclusion
    | Hue
    | Saturation
    | Color
    | Luminosity


default : Blend
default = Overlay


all : List Blend
all = forCanvas


-- https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation
forCanvas : List Blend
forCanvas =
    [ SourceOver
    , SourceIn
    , SourceOut
    , SourceAtop
    , DestinationOver
    , DestinationIn
    , DestinationOut
    , DestinationAtop
    , Lighter
    , Copy
    , Xor
    , Multiply
    , Screen
    , Overlay
    , Darken
    , Lighten
    , ColorDodge
    , ColorBurn
    , HardLight
    , SoftLight
    , Difference
    , Exclusion
    , Hue
    , Saturation
    , Color
    , Luminosity
    ]


forCss : List Blend
forCss =
    [ Normal
    , Multiply
    , Screen
    , Overlay
    , Darken
    , Lighten
    , ColorDodge
    , ColorBurn
    , HardLight
    , SoftLight
    , Difference
    , Exclusion
    , Hue
    , Saturation
    , Color
    , Luminosity
    ]


basicOnes : List Blend
basicOnes =
    [ SourceOver
    , Lighter
    , Darken
    , Lighten
    , Multiply
    , Screen
    , Overlay
    , ColorDodge
    , ColorBurn
    , HardLight
    , SoftLight
    , Difference
    , Exclusion
    ]


toString : Blend -> String
toString blend =
    case blend of
        Normal          -> "normal"
        SourceOver      -> "source-over"
        SourceIn        -> "source-in"
        SourceOut       -> "source-out"
        SourceAtop      -> "source-atop"
        DestinationOver -> "destination-over"
        DestinationIn   -> "destination-in"
        DestinationOut  -> "destination-out"
        DestinationAtop -> "destination-atop"
        Lighter         -> "lighter"
        Copy            -> "copy"
        Xor             -> "xor"
        Darken          -> "darken"
        Lighten         -> "lighten"
        Multiply        -> "multiply"
        Screen          -> "screen"
        Overlay         -> "overlay"
        ColorDodge      -> "color-dodge"
        ColorBurn       -> "color-burn"
        HardLight       -> "hard-light"
        SoftLight       -> "soft-light"
        Difference      -> "difference"
        Exclusion       -> "exclusion"
        Hue             -> "hue"
        Saturation      -> "saturation"
        Color           -> "color"
        Luminosity      -> "luminosity"


toLabel : Blend -> String
toLabel =
    toString >> String.replace "-" " "


fromString : String -> Maybe Blend
fromString str =
    case str of
        "source-over" -> Just SourceOver
        "source-in" -> Just SourceIn
        "source-out" -> Just SourceOut
        "source-atop" -> Just SourceAtop
        "destination-over" -> Just DestinationOver
        "destination-in" -> Just DestinationIn
        "destination-out" -> Just DestinationOut
        "destination-atop" -> Just DestinationAtop
        "lighter" -> Just Lighter
        "copy" -> Just Copy
        "xor" -> Just Xor
        "darken" -> Just Darken
        "lighten" -> Just Lighten
        "multiply" -> Just Multiply
        "screen" -> Just Screen
        "overlay" -> Just Overlay
        "color-dodge" -> Just ColorDodge
        "color-burn" -> Just ColorBurn
        "hard-light" -> Just HardLight
        "soft-light" -> Just SoftLight
        "difference" -> Just Difference
        "exclusion" -> Just Exclusion
        "hue" -> Just Hue
        "saturation" -> Just Saturation
        "color" -> Just Color
        "luminosity" -> Just Luminosity
        _ -> Nothing


equal : Blend -> Blend -> Bool
equal blendA blendB = toString blendA == toString blendB
