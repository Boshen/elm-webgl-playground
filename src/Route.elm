module Route exposing (Route(..), fromUrl, parser)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, string)


type Route
    = Mandelbrot
    | Rotate


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Mandelbrot (s "mandelbrot")
        , Parser.map Rotate (s "rotate")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
