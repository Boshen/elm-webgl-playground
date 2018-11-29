module Motion exposing (Model, Msg, defaultModel, init, subscriptions, update, view)

import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
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

       float random (in vec2 _st) {
          return fract(sin(dot(_st.xy, vec2(12.9898,78.233)))* 43758.5453123);
       }

       float noise (in vec2 _st) {
          vec2 i = floor(_st);
          vec2 f = fract(_st);

          float a = random(i);
          float b = random(i + vec2(1.0, 0.0));
          float c = random(i + vec2(0.0, 1.0));
          float d = random(i + vec2(1.0, 1.0));

          vec2 u = f * f * (3.0 - 2.0 * f);

          return mix(a, b, u.x) +
          (c - a)* u.y * (1.0 - u.x) +
          (d - b) * u.x * u.y;
       }

       float fbm ( in vec2 _st) {
          float v = 0.0;
          float a = 0.5;
          vec2 shift = vec2(100.0);
          mat2 rot = mat2(cos(0.5), sin(0.5),
          -sin(0.5), cos(0.50));
          for (int i = 0; i < 5; ++i) {
            v += a * noise(_st);
            _st = rot * _st * 2.0 + shift;
            a *= 0.5;
          }
          return v;
       }

       void main() {
          vec2 st = gl_FragCoord.xy/u_resolution.xy*3.0;
          st += st * abs(sin(u_time*0.1)*3.0);

          vec3 color = vec3(0.0);

          vec2 q = vec2(0.);
          q.x = fbm( st + 0.00*u_time);
          q.y = fbm( st + vec2(1.0));

          vec2 r = vec2(0.);
          r.x = fbm( st + 1.0*q + vec2(1.7,9.2)+ 0.15*u_time );
          r.y = fbm( st + 1.0*q + vec2(8.3,2.8)+ 0.126*u_time);

          float f = fbm(st+r);

          color = mix(vec3(0.101961,0.619608,0.666667),
                      vec3(0.666667,0.666667,0.498039),
                      clamp((f*f)*4.0,0.0,1.0));

          color = mix(color,
                      vec3(0,0,0.164706),
                      clamp(length(q),0.0,1.0));

          color = mix(color,
                      vec3(0.666667,1,1),
                      clamp(length(r.x),0.0,1.0));

          gl_FragColor = vec4((f*f*f+.6*f*f+.5*f)*color,1.);
       }
    |]
