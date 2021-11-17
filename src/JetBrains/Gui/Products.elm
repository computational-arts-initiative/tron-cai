module JetBrains.Gui.Products exposing (products)


import Dict

import Tron exposing (Tron)
import Tron.Build as Tron
import Tron.Style.PanelShape exposing (..)

import JetBrains.Product as Products exposing (Products)


products : Products -> Tron Products.Product
products ps =
    Tron.choiceBy
        (ps
            |> Dict.toList
            |> List.filter (Tuple.first >> Products.hasIcon)
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << productIcon))
            |> Tron.toSet Tuple.first
        )
        Products.default
        (\(nameA, _) (nameB, _) -> nameA == nameB)
        Tuple.second
    -- |> Tron.shape (rows 4)
    -- |> Tron.expand


productIcon : ( String, Products.Product ) -> Tron.Face
productIcon ( product, _ ) =
    Tron.iconAt
        [ "assets"
        , "product-logos"
        , (product |> Products.iconName |> Maybe.withDefault "none") ++ ".svg"
        ]
