module JetBrains.WithTron exposing
    ( Program
    , CaiPorts(..)
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


type CaiPorts msg
    = None
    | SendProducts
        { toPort : List Product -> Cmd msg
        }


init : CaiPorts msg -> (Model, Cmd Action)
init option =
    ( { products = [] }
    , Kraken.requestProducts
        |> Cmd.map ProductsReady
    )


update : CaiPorts msg -> Action -> Tree () -> Model -> (Model, Cmd msg)
update option action _ model =
    case ( option, action ) of
        ( SendProducts { toPort }, ProductsReady (Ok products) ) ->
            (
                { model
                | products = products
                }
            , toPort products
            )
        _ -> ( model, Cmd.none )


outToStrings : { a | update : { b | labelPath : List String, stringValue : String } } -> ( List String, String )
outToStrings out =
    ( out.update.labelPath, out.update.stringValue )



silence : Cmd msg -> Cmd Action
silence = Cmd.map <| always NoOp


element
    :  ( CaiPorts msg, Communication.Ports msg )
    -> Render.Target
    -> (Tree () -> Model -> Tree ())
    -> Program
element ( caiPorts, ports ) target for =
    WithTron.element
        target
        (ports |> Communication.map (always NoOp))
        { for =
              \tree model ->
                  for tree model
                      |> Tron.lift
                      |> Tron.map (always NoOp)
        , init = always <| init caiPorts
        , view = \_ _ -> Html.div [] []
        , update =
            \msg tree model ->
                update caiPorts msg tree model
                   |> Tuple.mapSecond silence
        , subscriptions =
            \_ _ -> Sub.none
        }
