module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Mandelbrot
import Rotate
import Route
import Sphere
import Url exposing (Url)


type Model
    = Mandelbrot Mandelbrot.Model
    | Rotate Rotate.Model
    | Sphere Sphere.Model


type Msg
    = NoOp
    | GotMandelbrotMsg Mandelbrot.Msg
    | GotRotate Rotate.Msg
    | GotSphere Sphere.Msg


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case Route.fromUrl url of
        Just Route.Mandelbrot ->
            ( Mandelbrot Mandelbrot.defaultModel, Cmd.map GotMandelbrotMsg Mandelbrot.cmd )

        Just Route.Rotate ->
            ( Rotate Rotate.defaultModel, Cmd.map GotRotate Rotate.cmd )

        Just Route.Sphere ->
            ( Sphere Sphere.defaultModel, Cmd.map GotSphere Sphere.cmd )

        Nothing ->
            ( Mandelbrot Mandelbrot.defaultModel, Cmd.map GotMandelbrotMsg Mandelbrot.cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotMandelbrotMsg subMsg, Mandelbrot m ) ->
            Mandelbrot.update subMsg m
                |> updateWith Mandelbrot GotMandelbrotMsg model

        ( GotRotate subMsg, Rotate m ) ->
            Rotate.update subMsg m
                |> updateWith Rotate GotRotate model

        ( GotSphere subMsg, Sphere m ) ->
            Sphere.update subMsg m
                |> updateWith Sphere GotSphere model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


view : Model -> Document Msg
view model =
    case model of
        Mandelbrot m ->
            Mandelbrot.view m

        Rotate m ->
            Rotate.view m

        Sphere m ->
            Sphere.view m


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Mandelbrot m ->
            Sub.map GotMandelbrotMsg (Mandelbrot.subscriptions m)

        Rotate m ->
            Sub.map GotRotate (Rotate.subscriptions m)

        Sphere m ->
            Sub.map GotSphere (Sphere.subscriptions m)


main : Program {} Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }
