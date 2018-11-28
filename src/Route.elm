module Route exposing (Route(..), fromUrl, routes)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Mandelbrot
    | Rotate
    | Motion
    | Sphere


routes =
    [ "mandelbrot"
    , "rotate"
    , "motion"
    , "sphere"
    ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Mandelbrot (s "mandelbrot")
        , Parser.map Rotate (s "rotate")
        , Parser.map Motion (s "motion")
        , Parser.map Sphere (s "sphere")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
