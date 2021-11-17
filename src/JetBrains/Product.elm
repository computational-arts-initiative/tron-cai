module JetBrains.Product exposing
    ( Product
    , Products
    , default
    , decodeOne
    , decode
    , hasIcon
    , iconName
    )


import Dict exposing (Dict)
import Json.Decode as D


type alias Color = { r : Float, g : Float, b : Float}
type alias Palette = List Color
type alias Product = { code : String, palette : Palette }


type alias Products = Dict String Product


type alias Palettes =
    List
        { product : String
        , palette: Palette
        }


default = ( "", { code = "", palette = [] } )


decodeColor : D.Decoder Color
decodeColor
    = D.map3
            Color
            (D.field "r" D.int |> D.map (\r -> toFloat r / 255))
            (D.field "g" D.int |> D.map (\g -> toFloat g / 255))
            (D.field "b" D.int |> D.map (\b -> toFloat b / 255))

decodeOne : D.Decoder Product
decodeOne
    = D.map2
        (\code palette -> { code = code, palette = palette })
        (D.field "code" D.string)
        (D.field "palette" <| D.list decodeColor)


decode : D.Decoder Products
decode =
    D.field "products"
        <| D.dict decodeOne


hasIcon : String -> Bool
hasIcon product =
    case iconName product of
        Just _ -> True
        Nothing -> False


iconName : String -> Maybe String
iconName product =
    case product of
        "JetBrains" -> Just "logojb"
        "Space" -> Just "Space"
        "IntelliJ IDEA" -> Just "IntelliJ-IDEA"
        "PhpStorm" -> Just "PhpStorm"
        "PyCharm" -> Just "PyCharm"
        "RubyMine" -> Just "RubyMine"
        "WebStorm" -> Just "WebStorm"
        "CLion" -> Just "CLion"
        "DataGrip" -> Just "DataGrip"
        "DataSpell" -> Just "DataSpell"
        "AppCode" -> Just "AppCode"
        "GoLand" -> Just "GoLand"
        "ReSharper" -> Just "ReSharper"
        "ReSharper C++" -> Just "ReSharperCPP"
        "dotCover" -> Just "dotCover"
        "dotMemory" -> Just "dotMemory"
        "dotPeek" -> Just "dotPeek"
        "dotTrace" -> Just "dotTrace"
        "Rider" -> Just "Rider"
        "TeamCity" -> Just "TeamCity"
        "YouTrack" -> Just "YouTrack"
        "Upsource" -> Just "Upsource"
        "Hub" -> Just "Hub"
        "Kotlin" -> Just "Kotlin"
        "Mono" -> Just "Mono"
        "MPS" -> Just "MPS"
        "IntelliJ IDEA Edu" -> Just "IntelliJ-IDEA-Edu"
        "PyCharm Edu" -> Just "PyCharm-Edu"
        "Datalore" -> Just "Datalore"
        _ -> Nothing


collectPalettes : Products -> Palettes
collectPalettes =
    Dict.toList
        >> List.map
            (\(productName, productData) ->
                { product = productName
                , palette = productData.palette
                }
            )
        >> List.filter (.product >> hasIcon)
