module Mandelbrot exposing (Model, Msg, cmd, defaultModel, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Task
import Time exposing (Posix, posixToMillis)
import WebGL exposing (Mesh, Shader)


type alias Model =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    , targetZoomX : Float
    , targetZoomY : Float
    , zoom : Float
    , zooming : Bool
    , maxIterations : Int
    }


type Msg
    = GotViewport Viewport
    | Zoom Float Float
    | StopZoom
    | Zooming


maxIterations =
    2048


defaultModel : Model
defaultModel =
    { width = 0
    , height = 0
    , x = 0
    , y = 0
    , targetZoomX = 0
    , targetZoomY = 0
    , zoom = 4.0
    , zooming = False
    , maxIterations = maxIterations
    }


cmd : Cmd Msg
cmd =
    Task.perform GotViewport getViewport


init : flags -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            case msg of
                GotViewport { viewport } ->
                    { model
                        | width = viewport.width
                        , height = viewport.height
                    }

                Zoom offsetX offsetY ->
                    { model
                        | targetZoomX = model.x - model.zoom / 2 + (offsetX / model.width) * model.zoom
                        , targetZoomY = model.y + model.zoom / 2 - (offsetY / model.height) * model.zoom
                        , zooming = True
                    }

                StopZoom ->
                    { model
                        | zooming = False
                    }

                Zooming ->
                    { model
                        | x = model.x + 0.1 * (model.targetZoomX - model.x)
                        , y = model.y + 0.1 * (model.targetZoomY - model.y)
                        , zoom = model.zoom * 0.98
                    }
    in
    ( m, Cmd.none )


decodeDown =
    Decode.map2 Zoom
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onMouseDown decodeDown
        , onMouseUp (Decode.succeed StopZoom)
        , if model.zooming then
            onAnimationFrame (\_ -> Zooming)

          else
            Sub.none
        ]


view : Model -> Document msg
view model =
    let
        html =
            WebGL.toHtml
                [ width (round model.width)
                , height (round model.height)
                , style "display" "block"
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    mesh
                    (uniforms model)
                ]
    in
    { title = "mandelbrot"
    , body = [ html ]
    }


type alias Uniforms =
    { u_resolution : Vec2
    , u_zoomCenter : Vec2
    , u_zoomSize : Float
    , u_maxIterations : Int
    }


uniforms : Model -> Uniforms
uniforms model =
    { u_resolution = vec2 model.width model.height
    , u_zoomCenter = vec2 model.x model.y
    , u_zoomSize = model.zoom
    , u_maxIterations = model.maxIterations
    }


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Mesh


type alias Vertex =
    { position : Vec2
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec2 1 1)
          , Vertex (vec2 -1 1)
          , Vertex (vec2 -1 -1)
          )
        , ( Vertex (vec2 -1 -1)
          , Vertex (vec2 1 1)
          , Vertex (vec2 1 -1)
          )
        ]



-- Shaders


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        precision highp float;
        attribute vec2 position;
        void main () {
          gl_Position = vec4(position, 0.0, 1.0);
        }
    |]



-- http://www.iquilezles.org/www/articles/distancefractals/distancefractals.htm<Paste>


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
       precision highp float;
        uniform vec2 u_resolution;
        uniform vec2 u_zoomCenter;
        uniform float u_zoomSize;
        uniform int u_maxIterations;

        void main() {
          vec2 uv = gl_FragCoord.xy / u_resolution;
          vec2 c = u_zoomCenter + (uv * 4.0 - vec2(2.0)) * (u_zoomSize / 4.0);
          vec2 z = vec2(0, 0);
          vec2 dz = vec2(0, 0);
          float m2 = 0.0;
          float di = 0.0;

          for(int i = 0; i < 1000; i++) {
            if (m2 > 1024.0) {
              di = 0.0;
              break;
            }
            dz = 2.0 * vec2(z.x * dz.x - z.y * dz.y, z.x * dz.y + z.y * dz.x) + vec2(1.0, 0.0);
            z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + c;
            m2 = dot(z, z);
          }

          float d = 0.5 * sqrt(dot(z,z)/dot(dz,dz))*log(dot(z,z));
          if (di > 0.5) d = 0.0;
          float tz = 0.5 * cos(0.225);
          float zoo = pow(0.5, 13.0 * tz);
          d = clamp(pow(4.0 * d / zoo, 0.2), 0.0, 1.0);
          vec3 col = vec3(d);
          gl_FragColor = vec4(col, 1.0);
        }
    |]
