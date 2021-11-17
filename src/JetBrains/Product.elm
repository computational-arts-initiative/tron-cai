module JetBrains.Product exposing
    ( Product
    , empty
    , paletteOf, codeOf, nameOf
    , decodeMany
    , hasIcon, iconName
    , compare
    )


import Dict exposing (Dict)
import Json.Decode as D

import JetBrains.Palette as Palette exposing (Palette)


type Product = Product { code : String, name : String, palette : Palette }


empty : Product
empty = Product { code = "", name = "", palette = [] }


paletteOf : Product -> Palette
paletteOf (Product { palette }) = palette


codeOf : Product -> String
codeOf (Product { code }) = code


nameOf : Product -> String
nameOf (Product { name }) = name


decode_ : D.Decoder { code : String, palette : Palette }
decode_
    = D.map2
        (\code palette -> { code = code, palette = palette })
        (D.field "code" D.string)
        (D.field "palette" Palette.decode)


decodeMany : D.Decoder (List Product)
decodeMany =
    D.field "products"
        <| D.map Dict.values
        <| D.map (Dict.map
                      (\productName { code, palette } ->
                           Product { name = productName, code = code, palette = palette }
                      )
                 )
        <| D.dict decode_


hasIcon : Product -> Bool
hasIcon product =
    case iconName product of
        Just _ -> True
        Nothing -> False


iconName : Product -> Maybe String
iconName (Product { name }) =
    case name of
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


compare : Product -> Product -> Bool
compare productA productB =
    nameOf productA == nameOf productB
