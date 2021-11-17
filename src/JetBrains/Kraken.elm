module JetBrains.Kraken exposing
    ( requestProducts
    )


import Http

import JetBrains.Product as Product exposing (Product)


url : String
url = "https://kraken.labs.jb.gg/"


requestProducts : Cmd (Result Http.Error (List Product))
requestProducts =
    Http.get
        { url = url ++ "palettes/get_all"
        , expect = Http.expectJson identity Product.decodeMany
        }
