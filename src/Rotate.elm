module Rotate exposing (Model, Msg, defaultModel, init, subscriptions, update, view)

import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Task
import Types exposing (Dimension)
import WebGL exposing (Mesh, Shader)


type alias Model =
    { width : Float
    , height : Float
    , time : Float
    }


type Msg
    = Tick


defaultModel : Dimension -> Model
defaultModel { width, height } =
    { width = width
    , height = height
    , time = 0
    }


init : Dimension -> ( Model, Cmd Msg )
init dimension =
    ( defaultModel dimension, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            case msg of
                Tick ->
                    { model | time = model.time + 0.01 }
    in
    ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame (\_ -> Tick)


view : Model -> Html Msg
view model =
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


type alias Uniforms =
    { u_resolution : Vec2
    , u_time : Float
    }


uniforms : Model -> Uniforms
uniforms model =
    { u_resolution = vec2 model.width model.height
    , u_time = model.time
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
       uniform float u_time;

       vec3 rgb2hsb( in vec3 c ){
         vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
         vec4 p = mix(vec4(c.bg, K.wz),
         vec4(c.gb, K.xy),
         step(c.b, c.g));
         vec4 q = mix(vec4(p.xyw, c.r),
         vec4(c.r, p.yzx),
         step(p.x, c.r));
         float d = q.x - min(q.w, q.y);
         float e = 1.0e-10;
         return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)),
         d / (q.x + e),
         q.x);
       }

       vec3 hsb2rgb( in vec3 c ){
         vec3 rgb = clamp(abs(mod(c.x*6.0+vec3(0.0,4.0,2.0),
         6.0)-3.0)-1.0,
         0.0,
         1.0 );
         rgb = rgb*rgb*(3.0-2.0*rgb);
         return c.z * mix(vec3(1.0), rgb, c.y);
       }

        float TWO_PI = 3.1415926 * 2.0;

       void main() {
         vec2 st = gl_FragCoord.xy / u_resolution;
         float pct = smoothstep(0.0, abs(sin(0.5 + u_time)), distance(st, vec2(0.5)));
         vec3 color = hsb2rgb(vec3(pct, 0.5, 1.0));

         gl_FragColor = vec4(color, 1.0);
       }
    |]
