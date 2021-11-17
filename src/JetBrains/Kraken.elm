module JetBrains.Kraken exposing
    ( requestProducts
    )


import Http

import JetBrains.Product as Product exposing (Product)


url : String
url = "https://kraken.labs.jb.gg/"



{-
collectPalettes : Products -> Palettes
collectPalettes =
    Dict.toList
        >> List.map
            (\(productName, productData) ->
                { product = productName
                , palette = productData.palette
                }
            )
        >> List.filter (.product >> hasIcon) -}



requestProducts : Cmd (Result Http.Error (List Product))
requestProducts =
    Http.get
        { url = url ++ "palettes/get_all"
        , expect = Http.expectJson identity Product.decodeMany
        }
