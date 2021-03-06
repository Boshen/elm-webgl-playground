module Mandelbrot exposing (Model, Msg, defaultModel, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Dom exposing (..)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (on, onMouseUp)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Task
import Time exposing (Posix, posixToMillis)
import Types exposing (Dimension)
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
    = Zoom Float Float
    | StopZoom
    | Zooming


maxIterations =
    256


defaultModel : Dimension -> Model
defaultModel { width, height } =
    { width = width
    , height = height
    , x = 0
    , y = 0
    , targetZoomX = 0
    , targetZoomY = 0
    , zoom = 4.0
    , zooming = False
    , maxIterations = maxIterations
    }


init : Dimension -> ( Model, Cmd Msg )
init dimension =
    ( defaultModel dimension, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            case msg of
                Zoom offsetX offsetY ->
                    { model
                        | targetZoomX = model.x - model.zoom / 2 + (offsetX / model.width) * model.zoom
                        , targetZoomY = model.y + model.zoom / 2 - (offsetY / model.height) * model.zoom
                        , zooming = True
                        , maxIterations = maxIterations
                    }

                StopZoom ->
                    { model
                        | zooming = False
                        , maxIterations = maxIterations * (5 - ceiling model.zoom) * 5
                    }

                Zooming ->
                    { model
                        | x = model.x + 0.1 * (model.targetZoomX - model.x)
                        , y = model.y + 0.1 * (model.targetZoomY - model.y)
                        , zoom = model.zoom * 0.98
                        , maxIterations = maxIterations
                    }
    in
    ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.zooming then
        onAnimationFrame (\_ -> Zooming)

    else
        Sub.none


decodeDown =
    Decode.map2 Zoom
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width (round model.width)
        , height (round model.height)
        , on "mousedown" decodeDown
        , onMouseUp StopZoom
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms model)
        ]


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


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
       precision highp float;
       uniform vec2 u_resolution;
       uniform vec2 u_zoomCenter;
       uniform float u_zoomSize;
       uniform int u_maxIterations;

       vec3 hsb2rgb( in vec3 c ){
         vec3 rgb = clamp(abs(mod(c.x*6.0+vec3(0.0,4.0,2.0), 6.0)-3.0)-1.0, 0.0, 1.0);
         rgb = rgb*rgb*(3.0-2.0*rgb);
         return c.z * mix(vec3(1.0), rgb, c.y);
       }

       vec3 hsv_to_rgb(in vec3 hsv)
       {
         float h = hsv.x;
         float s = hsv.y;
         float v = hsv.z;
         if (v > 1.0) v = 1.0;
         float hp = h / 60.0;
         float c = v * s;
         float x = c * (1.0 - abs(mod(hp, 2.0) - 1.0));
         vec3 rgb = vec3(0.0);

         if (0.0 <= hp && hp < 1.0) rgb = vec3(c, x, 0.0);
         if (1.0 <= hp && hp < 2.0) rgb = vec3(x, c, 0.0);
         if (2.0 <= hp && hp < 3.0) rgb = vec3(0.0, c, x);
         if (3.0 <= hp && hp < 4.0) rgb = vec3(0.0, x, c);
         if (4.0 <= hp && hp < 5.0) rgb = vec3(x, 0.0, c);
         if (5.0 <= hp && hp < 6.0) rgb = vec3(c, 0.0, x);

         float m = v - c;
         rgb += m;
         return rgb;
       }

       float logBase = 1.0 / log(2.0);
       float logHalfBase = log(0.5) * logBase;

       void main() {
         vec2 uv = gl_FragCoord.xy / u_resolution;
         vec2 c = u_zoomCenter + (uv * 4.0 - vec2(2.0)) * (u_zoomSize / 4.0);
         float cr = c.x;
         float ci = c.y;
         float zr = 0.0;
         float zi = 0.0;
         float tr = 0.0;
         float ti = 0.0;
         float n  = 0.0;
         float steps = float(u_maxIterations);

         for(int i = 0; i < 10000; i++) {
           zi = 2.0 * zr * zi + ci;
           zr = tr - ti + cr;
           tr = zr * zr;
           ti = zi * zi;
           n = float(i);
           if (n >= steps || tr + ti >= 4.0) {
             break;
           }
         }

         if (n >= steps) {
           gl_FragColor = vec4(1.0);
         } else {
           for (int e = 0; e < 4; e++) {
             zi = 2.0 * zr * zi + ci;
             zr = tr - ti + cr;
             tr = zr * zr;
             ti = zi * zi;
           }
           float v = 5.0 + n - logHalfBase - log(log(tr + ti)) * logBase;
           vec3 color = hsv_to_rgb(vec3(360.0 * v / steps, 0.8, 20.0 * v / steps));
           gl_FragColor = vec4(color, 1.0);
         }

       }
    |]
