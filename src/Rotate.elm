module Rotate exposing (Model, Msg, cmd, defaultModel, init, subscriptions, update, view)

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
    }


type Msg
    = GotViewport Viewport


defaultModel : Model
defaultModel =
    { width = 0
    , height = 0
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
    in
    ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
    { title = "rotate"
    , body = [ html ]
    }


type alias Uniforms =
    { u_resolution : Vec2
    }


uniforms : Model -> Uniforms
uniforms model =
    { u_resolution = vec2 model.width model.height
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


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
       precision highp float;
       uniform vec2 u_resolution;

       void main() {
         vec2 uv = gl_FragCoord.xy / u_resolution;
         gl_FragColor = vec4(uv, 0.5, 1.0);
       }
    |]
