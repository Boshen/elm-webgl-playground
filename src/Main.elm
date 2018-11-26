module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Mandelbrot
import Url exposing (Url)


type Model
    = Mandelbrot Mandelbrot.Model


type Msg
    = NoOp
    | GotMandelbrotMsg Mandelbrot.Msg


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Mandelbrot Mandelbrot.defaultModel, Cmd.map GotMandelbrotMsg Mandelbrot.cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotMandelbrotMsg subMsg, Mandelbrot m ) ->
            Mandelbrot.update subMsg m
                |> updateWith Mandelbrot GotMandelbrotMsg model

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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Mandelbrot m ->
            Sub.map GotMandelbrotMsg (Mandelbrot.subscriptions m)


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
