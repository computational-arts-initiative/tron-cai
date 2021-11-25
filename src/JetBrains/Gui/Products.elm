module JetBrains.Gui.Products exposing
    (products, whichProduct, products_)


import Tron exposing (Tron)
import Tron.Build as Tron
import Tron.Style.PanelShape exposing (..)

import WithTron.ValueAt as Value

import JetBrains.Product as Product exposing (Product)



products : (Product -> Tron.Face) -> List Product -> Tron Product
products toIcon = productsM toIcon >> Tron.map (Maybe.withDefault Product.empty)


products_ : List Product -> Tron Product
products_ = products productIcon


productsM : (Product -> Tron.Face) -> List Product -> Tron (Maybe Product)
productsM toIcon ps =
    Tron.choiceBy
        (ps
            --|> List.filter Product.hasIcon
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << toIcon))
            |> List.map (Tron.map Just)
            |> Tron.toSet (Maybe.map Product.nameOf >> Maybe.withDefault "--")
        )
        Nothing
        (\p1 p2 -> Maybe.map2 Product.equal p1 p2 |> Maybe.withDefault False)
        identity
    -- |> Tron.shape (rows 4)
    -- |> Tron.expand


productsM_ : List Product -> Tron (Maybe Product)
productsM_ = productsM productIcon



productIcon : Product -> Tron.Face
productIcon product =
    Tron.iconAt
        [ "assets"
        , "product-logos"
        , (product |> Product.iconName |> Maybe.withDefault "none") ++ ".svg"
        ]


whichProduct : List Product -> List String -> Value.Decoder Product
whichProduct =
    Value.choiceOf
