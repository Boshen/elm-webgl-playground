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
    }


uniforms : Model -> Uniforms
uniforms model =
    { u_resolution = vec2 (toFloat model.width) (toFloat model.height)
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
        attribute vec2 position;
        void main () {
          gl_Position = vec4(position, 0.0, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec2 u_resolution;
        uniform float u_time;

        void main () {
          vec2 st = gl_FragCoord.xy / u_resolution.xy;
          vec3 color = vec3(st.x * abs(sin(u_time)), st.y * abs(cos(u_time)), 1);
          gl_FragColor = vec4(color, 1.0);
        }
    |]
