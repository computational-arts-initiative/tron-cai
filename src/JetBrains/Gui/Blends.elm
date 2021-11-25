module JetBrains.Gui.Blends exposing
    (blends, blends_, whichBlend)



import Tron exposing (Tron)
import Tron.Build as Tron
import Tron.Style.PanelShape exposing (..)

import WithTron.ValueAt as Value

import JetBrains.Blend as Blend exposing (Blend)



blends : (Blend -> Tron.Face) -> List Blend -> Tron Blend
blends toIcon = blendsM toIcon >> Tron.map (Maybe.withDefault Blend.default)


blends_ : List Blend -> Tron Blend
blends_ = blends blendIcon


blendsM : (Blend -> Tron.Face) -> List Blend -> Tron (Maybe Blend)
blendsM toIcon ps =
    Tron.choiceBy
        (ps
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << toIcon))
            |> List.map (Tron.map Just)
            |> Tron.toSet (Maybe.map Blend.toString >> Maybe.withDefault "--")
        )
        Nothing
        (\p1 p2 -> Maybe.map2 Blend.equal p1 p2 |> Maybe.withDefault False)
        identity
    -- |> Tron.shape (rows 4)
    -- |> Tron.expand


blendsM_ : List Blend -> Tron (Maybe Blend)
blendsM_ = blendsM blendIcon



blendIcon : Blend -> Tron.Face
blendIcon blend =
    Tron.iconAt
        [ "assets"
        , "blend-icons"
        , (blend |> Blend.toString) ++ ".svg"
        ]


whichBlend : List Blend -> List String -> Value.Decoder Blend
whichBlend =
    Value.choiceOf
