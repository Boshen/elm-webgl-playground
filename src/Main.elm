module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Debug
import Mandelbrot
import Rotate
import Route
import Url exposing (Url)


type Model
    = Mandelbrot Mandelbrot.Model
    | Rotate Rotate.Model


type Msg
    = NoOp
    | GotMandelbrotMsg Mandelbrot.Msg
    | GotRotate Rotate.Msg


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case Debug.log "asdf" (Route.fromUrl url) of
        Just Route.Mandelbrot ->
            ( Mandelbrot Mandelbrot.defaultModel, Cmd.map GotMandelbrotMsg Mandelbrot.cmd )

        Just Route.Rotate ->
            ( Rotate Rotate.defaultModel, Cmd.map GotRotate Rotate.cmd )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Mandelbrot m ->
            Sub.map GotMandelbrotMsg (Mandelbrot.subscriptions m)

        Rotate m ->
            Sub.map GotRotate (Rotate.subscriptions m)


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
