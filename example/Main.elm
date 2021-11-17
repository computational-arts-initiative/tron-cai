module Main exposing (main)


import Browser

import WithTron
import Tron.Tree as Tree
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock

import JetBrains.Gui.Products as P


main : WithTron.Program () () ()
main =
    Browser.element
        <| WithTron.justUi
            (Render.toHtml Dock.center Theme.Dark)
            (always <| Tree.toUnit <| P.products)
