module Sphere exposing (Attributes, Model, Msg(..), Uniforms, Varyings, cmd, createIndex, createVertex, defaultModel, fragmentShader, init, latitudeBands, longitudeBands, radius, sphereMesh, subscriptions, uniforms, update, vertexShader, view)

import Browser exposing (Document)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Value)
import List
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random exposing (..)
import Task
import Time exposing (Posix, posixToMillis)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings.DepthTest


type alias Model =
    { width : Float
    , height : Float
    , time : Float
    , spheres : List Sphere
    }


type alias Sphere =
    { x : Float
    , y : Float
    , z : Float
    , v : Float
    }


type Msg
    = GotViewport Viewport
    | Tick Float
    | Randoms (List ( Float, Float ))


defaultModel : Model
defaultModel =
    { width = 0
    , height = 0
    , time = 0
    , spheres = []
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

                Tick dt ->
                    { model
                        | time = model.time + dt
                        , spheres = List.map (updateSphere model.time) model.spheres
                    }

                Randoms randoms ->
                    { model
                        | spheres = createSpheres randoms model.width model.height
                    }

        c =
            case msg of
                GotViewport _ ->
                    Random.generate Randoms (Random.list 1000 <| Random.pair (Random.float -2 2) (Random.float 0 1))

                _ ->
                    Cmd.none
    in
    ( m, c )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta (\dt -> Tick (dt / 1000))


view : Model -> Document msg
view model =
    let
        html =
            WebGL.toHtmlWith
                [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 ]
                [ width (round model.width)
                , height (round model.height)
                , style "display" "block"
                ]
                (List.map createSphereEntity model.spheres)
    in
    { title = "sphere"
    , body = [ html ]
    }


type alias Uniforms =
    { model : Mat4
    , perspective : Mat4
    , view : Mat4
    }


createSphereEntity sphere =
    WebGL.entityWith
        [ WebGL.Settings.DepthTest.default ]
        vertexShader
        fragmentShader
        sphereMesh
        (uniforms sphere)


createSpheres randoms width height =
    List.map2
        (\( x, z ) v -> Sphere x 0 z (toFloat v))
        randoms
        (List.range 1 1)


updateSphere : Float -> Sphere -> Sphere
updateSphere dt sphere =
    { sphere
        | x = sphere.x + cos dt * sphere.v * 0.04
        , z = sphere.z + sin dt * sphere.v * 0.05
    }


uniforms : Sphere -> Uniforms
uniforms { x, y, z } =
    { model =
        Mat4.identity
            |> Mat4.translate3 x y z
            -- |> Mat4.rotate 0 (vec3 0 0 0)
            |> Mat4.scale3 0.3 0.3 0.3
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , view = Mat4.makeLookAt (vec3 0 1 -10) (vec3 0 0 0) (vec3 0 1 0)
    }



-- Mesh


type alias Attributes =
    { position : Vec3
    , color : Vec3
    , normal : Vec3
    }


type alias Varyings =
    { vlight : Vec3
    }


latitudeBands : Int
latitudeBands =
    50


longitudeBands : Int
longitudeBands =
    50


radius : Float
radius =
    2.0


sphereMesh : Mesh Attributes
sphereMesh =
    let
        attributes =
            List.concatMap
                (\l -> List.map (createVertex l) (List.range 0 longitudeBands))
                (List.range 0 latitudeBands)

        vertices =
            List.concat <|
                List.concatMap
                    (\l -> List.map (createIndex l) (List.range 0 (longitudeBands - 1)))
                    (List.range 0 (latitudeBands - 1))
    in
    WebGL.indexedTriangles attributes vertices


createVertex : Int -> Int -> Attributes
createVertex lat long =
    let
        latNumber =
            toFloat lat

        longNumber =
            toFloat long

        latBands =
            toFloat latitudeBands

        longBands =
            toFloat longitudeBands

        theta =
            latNumber * pi / latBands

        sinTheta =
            sin theta

        cosTheta =
            cos theta

        phi =
            longNumber * 2 * pi / longBands

        sinPhi =
            sin phi

        cosPhi =
            cos phi

        x =
            cosPhi * sinTheta

        y =
            cosTheta

        z =
            sinPhi * sinTheta

        u =
            1 - (longNumber / longBands)

        v =
            1 - (latNumber / latBands)
    in
    Attributes
        (vec3 (radius * x) (radius * y) (radius * z))
        (vec3 1.0 1.0 1.0)
        (vec3 x y z)


createIndex : Int -> Int -> List ( Int, Int, Int )
createIndex latNumber longNumber =
    let
        first =
            (latNumber * (longitudeBands + 1)) + longNumber

        second =
            first + longitudeBands + 1
    in
    [ ( first, second, first + 1 ), ( second, second + 1, first + 1 ) ]


vertexShader : Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;
        uniform mat4 perspective;
        uniform mat4 view;
        uniform mat4 model;
        varying vec3 vlight;

        vec4 lightPosition = vec4(10.0, 20.0, -20.0, 1.0);
        vec3 ld = vec3(1.0, 1.0, 1.0);
        vec3 kd = vec3(0.9, 0.5, 0.3);

        void main () {
          vec3 tnorm = normalize(normal);
          mat4 mvp = perspective * view * model;
          vec4 eyeCoords = mvp * vec4(position, 1.0);
          vec3 s = normalize(vec3(lightPosition - eyeCoords));

          vlight = ld * kd * max(dot(s, tnorm), 0.0);

          gl_Position =  mvp * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vlight;

        void main () {
          gl_FragColor = vec4(vlight, 1.0);
        }
    |]
