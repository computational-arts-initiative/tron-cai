module JetBrains.WithTron exposing
    ( Program
    , PortProduct
    , Option(..)
    , element
    , outToStrings
    )


import Http
import Html
import Color
import Task

import Tron.Tree exposing (Tree)
import Tron.Tree.Expose.Data as Exp
import Tron.Option.Render as Render
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron as Tron exposing (Tron)

import WithTron

import JetBrains.Product as Product exposing (Product)
import JetBrains.Kraken as Kraken


type alias Program model msg = WithTron.Program () ( model, Model ) (Action msg)


type Action msg
    = ProductsReady (Result Http.Error (List Product))
    | ToUser msg


type alias PortProduct =
    { name : String, palette : List { red : Float, green : Float, blue : Float, alpha : Float }}


type alias Model =
    { products : List Product }


type Option msg
    = Products
         { onReceive : List Product -> msg
         , onError : Http.Error -> msg
         , toPort : List PortProduct -> Cmd msg
         }
    | Blends
         {}
    | ListenUpdates
         { transmit : Exp.Out -> Cmd msg }



init : List (Option msg) -> (Model, Cmd (Action msg))
init options =
    ( { products = [] }
    , Kraken.requestProducts
        |> Cmd.map ProductsReady
    )


update : List (Option msg) -> Action msg -> Tree () -> Model -> (Model, Cmd msg)
update options action _ model =
    options
        |> List.foldl
           (\option ( prevModel, prevCmd ) ->
                case ( option, action ) of
                    ( Products { onReceive, toPort }, ProductsReady (Ok products) ) ->
                        (
                            { prevModel
                            | products = products
                            }
                        , Cmd.batch
                            [ prevCmd
                            , onReceive products
                                 |> Task.succeed
                                 |> Task.perform identity
                            , toPort <| List.map productToPort <| products
                            ]
                        )
                    _ -> ( prevModel, prevCmd )
           )
           ( model, Cmd.none )


outToStrings : { a | update : { b | labelPath : List String, stringValue : String } } -> ( List String, String )
outToStrings out =
    ( out.update.labelPath, out.update.stringValue )


element
    :  List (Option msg)
    -> Render.Target
    -> (Tree () -> Model -> Tron msg)
    -> Program () msg
element options target for =
    WithTron.element
        target
        (options
            |> List.foldl
                (\option prev ->
                     case option of
                         ListenUpdates { transmit }
                             -> Communication.sendJson
                                   { ack = always Cmd.none
                                   , transmit = transmit >> Cmd.map ToUser
                                   }
                         _ -> prev
                )
                Communication.none
        )
        { for = \tree ( _, model ) -> for tree model |> Tron.map ToUser
        , init = always <| Tuple.mapFirst (Tuple.pair ()) <| init options
        , view = \_ _ -> Html.div [] []
        , update =
            \msg tree ( _, model ) ->
                update options msg tree model
                   |> Tuple.mapSecond (Cmd.map ToUser)
                   |> Tuple.mapFirst (Tuple.pair ())
        , subscriptions =
            \_ _ -> Sub.none
        }


productToPort : Product -> PortProduct
productToPort product =
    { name = Product.nameOf product
    , palette = Product.paletteOf product
                     |> List.map Color.toRgba
    }
