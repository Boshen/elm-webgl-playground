module Main exposing (Uniforms, Vertex, fragmentShader, main, mesh, vertexShader, view)

import Browser
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Task
import Time exposing (Posix, posixToMillis)
import WebGL exposing (Mesh, Shader)


type alias Model =
    { width : Int
    , height : Int
    , time : Float
    }


type Msg
    = GotViewport Viewport
    | Tick


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { width = 0, height = 0, time = 0 }, Task.perform GotViewport getViewport )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            case msg of
                GotViewport { viewport } ->
                    { model
                        | width = round viewport.width
                        , height = round viewport.height
                    }

                Tick ->
                    { model
                        | time = model.time + 1
                    }
    in
    ( m, Cmd.none )


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Time.every 1000 (\_ -> Tick)
        }


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width model.width
        , height model.height
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
    , u_zoomCenter : Vec2
    , u_zoomSize : Float
    , u_maxIterations : Int
    }


uniforms : Model -> Uniforms
uniforms model =
    { u_resolution = vec2 800.0 800.0
    , u_time = 0 -- model.time
    , u_zoomCenter = vec2 0.0 0.0
    , u_zoomSize = 4
    , u_maxIterations = 500
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

        void main() {
          vec2 uv = gl_FragCoord.xy / u_resolution;
          vec2 c = u_zoomCenter + (uv * 4.0 - vec2(2.0)) * (u_zoomSize / 4.0);
          vec2 z;

          for(int i = 0; i < 10000; i++) {
            if (i > u_maxIterations) break;
            z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + c;
            gl_FragColor = vec4(vec3((float(i) - log(log(length(z)))) / 64.0), 1);
            if (length(z) > 2.0) return;
          }

          gl_FragColor = vec4(vec3(0), 1);
        }
    |]
