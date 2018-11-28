module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Mandelbrot
import Motion
import Rotate
import Route
import Sphere
import Url exposing (Url)


type alias Model =
    { key : Key
    , subModel : SubModel
    }


type SubModel
    = Mandelbrot Mandelbrot.Model
    | Rotate Rotate.Model
    | Motion Motion.Model
    | Sphere Sphere.Model
    | NoModel


type Msg
    = NoOp
    | ClickedLink UrlRequest
    | ChangedUrl Url
    | GotMandelbrotMsg Mandelbrot.Msg
    | GotRotate Rotate.Msg
    | GotMotion Motion.Msg
    | GotSphere Sphere.Msg


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( subModel, subCmd ) =
            case Route.fromUrl url of
                Just Route.Mandelbrot ->
                    ( Mandelbrot Mandelbrot.defaultModel, Cmd.map GotMandelbrotMsg Mandelbrot.cmd )

                Just Route.Rotate ->
                    ( Rotate Rotate.defaultModel, Cmd.map GotRotate Rotate.cmd )

                Just Route.Motion ->
                    ( Motion Motion.defaultModel, Cmd.map GotMotion Motion.cmd )

                Just Route.Sphere ->
                    ( Sphere Sphere.defaultModel, Cmd.map GotSphere Sphere.cmd )

                Nothing ->
                    ( NoModel, Cmd.none )
    in
    ( Model key subModel, subCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.subModel ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.replaceUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( ChangedUrl url, _ ) ->
            init {} url model.key

        ( GotMandelbrotMsg subMsg, Mandelbrot m ) ->
            Mandelbrot.update subMsg m
                |> updateWith Mandelbrot GotMandelbrotMsg model

        ( GotRotate subMsg, Rotate m ) ->
            Rotate.update subMsg m
                |> updateWith Rotate GotRotate model

        ( GotMotion subMsg, Motion m ) ->
            Motion.update subMsg m
                |> updateWith Motion GotMotion model

        ( GotSphere subMsg, Sphere m ) ->
            Sphere.update subMsg m
                |> updateWith Sphere GotSphere model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> SubModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | subModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )


view : Model -> Document Msg
view model =
    let
        right =
            case model.subModel of
                Mandelbrot m ->
                    Html.map GotMandelbrotMsg <| Mandelbrot.view m

                Rotate m ->
                    Rotate.view m

                Motion m ->
                    Motion.view m

                Sphere m ->
                    Sphere.view m

                _ ->
                    div [] []

        body =
            main_
                []
                [ navView, right ]
    in
    { title = ""
    , body = [ body ]
    }


navView =
    header []
        [ ul
            []
            (List.map linkView Route.routes)
        ]


linkView url =
    li [] [ a [ href ("/" ++ url) ] [ text url ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.subModel of
        Mandelbrot m ->
            Sub.map GotMandelbrotMsg (Mandelbrot.subscriptions m)

        Rotate m ->
            Sub.map GotRotate (Rotate.subscriptions m)

        Motion m ->
            Sub.map GotMotion (Motion.subscriptions m)

        Sphere m ->
            Sub.map GotSphere (Sphere.subscriptions m)

        _ ->
            Sub.none


main : Program {} Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
