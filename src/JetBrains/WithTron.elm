module JetBrains.WithTron exposing
    ( Program
    , Option(..)
    , element
    , outToStrings
    )


import Http
import Html

import Tron.Tree exposing (Tree)
import Tron.Tree.Expose.Data as Exp
import Tron.Option.Render as Render
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron as Tron exposing (Tron)

import WithTron

import JetBrains.Product as Product exposing (Product)
import JetBrains.Kraken as Kraken


type alias Program = WithTron.Program () Model Action


type Action
    = NoOp
    | ProductsReady (Result Http.Error (List Product))


type alias Model =
    { products : List Product }


type Option msg
    = Products
         { toPort : List Product -> Cmd msg
         }
    | ListenUpdates
         { transmit : Exp.Out -> Cmd msg
         }



init : List (Option msg) -> (Model, Cmd Action)
init options =
    ( { products = [] }
    , Kraken.requestProducts
        |> Cmd.map ProductsReady
    )


update : List (Option msg) -> Action -> Tree () -> Model -> (Model, Cmd msg)
update options action _ model =
    options
        |> List.foldl
           (\option ( prevModel, prevCmd ) ->
                case ( option, action ) of
                    ( Products { toPort }, ProductsReady (Ok products) ) ->
                        (
                            { prevModel
                            | products = products
                            }
                        , Cmd.batch
                            [ prevCmd
                            , toPort products
                            ]
                        )
                    _ -> ( prevModel, prevCmd )
           )
           ( model, Cmd.none )


outToStrings : { a | update : { b | labelPath : List String, stringValue : String } } -> ( List String, String )
outToStrings out =
    ( out.update.labelPath, out.update.stringValue )



silence : Cmd msg -> Cmd Action
silence = Cmd.map <| always NoOp


element
    :  List (Option msg)
    -> Render.Target
    -> (Tree () -> Model -> Tree ())
    -> Program
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
                                   , transmit = transmit >> silence
                                   }
                         _ -> prev
                )
                Communication.none
        )
        { for =
              \tree model ->
                  for tree model
                      |> Tron.lift
                      |> Tron.map (always NoOp)
        , init = always <| init options
        , view = \_ _ -> Html.div [] []
        , update =
            \msg tree model ->
                update options msg tree model
                   |> Tuple.mapSecond silence
        , subscriptions =
            \_ _ -> Sub.none
        }
