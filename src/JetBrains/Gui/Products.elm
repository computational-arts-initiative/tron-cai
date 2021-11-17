module JetBrains.Gui.Products exposing (products)


import Tron exposing (Tron)
import Tron.Build as Tron

import JetBrains.Product as Product exposing (Product)


products : Tron (Maybe Product)
products = Tron.root []
