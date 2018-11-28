module Sphere exposing (Model, Msg(..), cmd, defaultModel, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import List
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random exposing (..)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Settings.DepthTest


type Msg
    = GotViewport Viewport
    | Tick Float
    | Randoms (List ( Float, Float ))


type alias Model =
    { width : Float
    , height : Float
    , time : Float
    , spheres : List Sphere
    , light : Sphere
    , directionalLight : Vec3
    , directionalColor : Vec3
    , camera : Vec3
    }


type alias Sphere =
    { x : Float
    , y : Float
    , z : Float
    , v : Float -- velocity
    , r : Float -- radius
    , c : Vec3 -- color
    }


type alias SphereUniforms =
    { model : Mat4
    , perspective : Mat4
    , view : Mat4
    , lightPos : Vec3
    , lightColor : Vec3
    , directionalLight : Vec3
    , directionalColor : Vec3
    , color : Vec3
    , camera : Vec3
    }


type alias LightUniforms =
    { model : Mat4
    , perspective : Mat4
    , view : Mat4
    , color : Vec3
    , camera : Vec3
    , directionalLight : Vec3
    , directionalColor : Vec3
    }


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias SphereVaryings =
    { v_normal : Vec3
    , v_fragPos : Vec3
    }


type alias LightVaryings =
    { v_normal : Vec3
    , v_fragPos : Vec3
    }


defaultModel : Model
defaultModel =
    { width = 0
    , height = 0
    , time = 0
    , spheres = []
    , light = { x = 0, y = 0, z = 0, v = 0, r = 0.1, c = vec3 1 1 0 }
    , camera = vec3 0 0 -10
    , directionalLight = vec3 0 0 1
    , directionalColor = vec3 0.3 0.3 0.3
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


view : Model -> Html msg
view model =
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 ]
        [ width (round model.width)
        , height (round model.height)
        , style "display" "block"
        ]
        (createLightEntity model :: List.map (createSphereEntity model) model.spheres)


createSphereEntity model sphere =
    WebGL.entityWith
        [ WebGL.Settings.DepthTest.default ]
        sphereVertexShader
        sphereFragmentShader
        sphereMesh
        (sphereUniforms model sphere)


createLightEntity model =
    WebGL.entityWith
        [ WebGL.Settings.DepthTest.default ]
        lightVertexShader
        lightFragmentShader
        sphereMesh
        (lightUniforms model)


createSpheres : List ( Float, Float ) -> Float -> Float -> List Sphere
createSpheres randoms width height =
    [ { x = 0
      , y = -0.05
      , z = 0
      , v = 0.1
      , r = 0.3
      , c = vec3 1 0 0
      }
    ]


updateSphere : Float -> Sphere -> Sphere
updateSphere t sphere =
    { sphere
        | x = sphere.x + cos t * sphere.v
        , z = sphere.z + sin t * sphere.v
    }


sphereUniforms : Model -> Sphere -> SphereUniforms
sphereUniforms { light, camera, directionalLight, directionalColor } sphere =
    { model =
        Mat4.identity
            |> Mat4.translate3 sphere.x sphere.y sphere.z
            -- |> Mat4.rotate 0 (vec3 0 0 0)
            |> Mat4.scale3 sphere.r sphere.r sphere.r
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , view = Mat4.makeLookAt camera (vec3 0 0 0) (vec3 0 1 0)
    , lightPos = vec3 light.x light.y light.z
    , lightColor = light.c
    , color = sphere.c
    , camera = camera
    , directionalColor = directionalColor
    , directionalLight = directionalLight
    }


lightUniforms : Model -> LightUniforms
lightUniforms { light, camera, directionalLight, directionalColor } =
    { model =
        Mat4.identity
            |> Mat4.translate3 light.x light.y light.z
            -- |> Mat4.rotate 0 (vec3 0 0 0)
            |> Mat4.scale3 light.r light.r light.r
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , view = Mat4.makeLookAt (vec3 0 1 -10) (vec3 0 0 0) (vec3 0 1 0)
    , color = light.c
    , camera = camera
    , directionalColor = directionalColor
    , directionalLight = directionalLight
    }



-- Mesh


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


sphereVertexShader : Shader Attributes SphereUniforms SphereVaryings
sphereVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 perspective;
        uniform mat4 view;
        uniform mat4 model;

        varying vec3 v_normal;
        varying vec3 v_fragPos;

        void main () {
          vec4 pos = vec4(position, 1.0);
          gl_Position =  perspective * view * model * pos;

          v_normal = normal;
          v_fragPos = vec3(model * pos);
        }
    |]


sphereFragmentShader : Shader {} SphereUniforms SphereVaryings
sphereFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        uniform vec3 lightPos;
        uniform vec3 lightColor;
        uniform vec3 camera;
        uniform vec3 directionalLight;
        uniform vec3 directionalColor;

        varying vec3 v_normal;
        varying vec3 v_fragPos;


        void main () {
          vec3 norm = normalize(v_normal);
          vec3 lightDir = normalize(lightPos - v_fragPos);
          vec3 viewDir = normalize(camera - v_fragPos);
          vec3 reflectDir = reflect(-lightDir, norm);

          vec3 ambient = vec3(0.1, 0.1, 0.1);
          vec3 diffuse = max(0.0, dot(norm, lightDir)) * lightColor;
          vec3 specular = 0.5 * pow(max(dot(viewDir, reflectDir), 0.0), 32.0) * lightColor;
          vec3 directional = max(0.0, dot(norm, -directionalLight)) * directionalColor;

          vec3 result = (ambient + diffuse + specular + directional) * color;
          gl_FragColor = vec4(result, 1.0);
        }
    |]


lightVertexShader : Shader Attributes LightUniforms LightVaryings
lightVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 perspective;
        uniform mat4 view;
        uniform mat4 model;

        varying vec3 v_normal;
        varying vec3 v_fragPos;

        void main () {
          vec4 pos = vec4(position, 1.0);
          gl_Position =  perspective * view * model * pos;

          v_normal = normal;
          v_fragPos = vec3(model * pos);
        }
    |]


lightFragmentShader : Shader {} LightUniforms LightVaryings
lightFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        uniform vec3 directionalLight;
        uniform vec3 directionalColor;

        varying vec3 v_normal;
        varying vec3 v_fragPos;

        void main () {
          vec3 norm = normalize(v_normal);

          vec3 ambient = vec3(0.8, 0.8, 0.8);
          vec3 directional = max(0.0, dot(norm, -directionalLight)) * directionalColor;

          vec3 result = (ambient + directional) * color;
          gl_FragColor = vec4(result, 1.0);
        }
    |]
