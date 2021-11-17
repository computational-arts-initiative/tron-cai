port module Main exposing (main)


import Browser
import Http
import Html
import Color


import WithTron
import Tron exposing (Tron)
import Tron.Tree exposing (Tree)
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock


import JetBrains.Product as Product exposing (Product)
import JetBrains.Gui.Products as P
import JetBrains.Kraken as K


type alias Model = List Product


type Action
   = NoOp
   | ProductsReady (Result Http.Error Model)


for : Model -> Tron Action
for products =
    P.products_ products
        |> Tron.map (always NoOp)


init : flags -> ( Model, Cmd Action )
init _ = ( [], K.requestProducts |> Cmd.map ProductsReady )


update : Action -> Tree () -> Model -> ( Model, Cmd Action )
update msg _ model =
    case msg of
        ProductsReady (Ok products) ->
            ( products
            , sendProducts <| adaptProducts products
            )
        _ -> ( model, Cmd.none )


main : WithTron.Program () Model Action
main =
    WithTron.element
        (Render.toHtml Dock.bottomCenter Theme.dark)
        (Communication.sendStrings { transmit = sendUpdate })
        { for = always for
        , init = init
        , view = \_ _ -> Html.div [] []
        , update = update
        , subscriptions =
            \_ _ -> Sub.none
        }


type alias PortProduct =
    { name : String
    , palette : List { red : Float, green : Float, blue : Float, alpha : Float }
    }


adaptProducts : List Product -> List PortProduct
adaptProducts =
    List.map (\product ->
                  { name = Product.nameOf product
                  , palette = Product.paletteOf product |> List.map Color.toRgba
                  }
             )


port sendUpdate : ( List String, String ) -> Cmd msg


port sendProducts
    : List PortProduct -> Cmd msg
