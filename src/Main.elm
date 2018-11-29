module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Element)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
import Mandelbrot
import Motion
import Result exposing (..)
import Rotate
import Route
import Sphere
import Task
import Tuple
import Types exposing (Dimension)
import Url exposing (Url)


type alias Model =
    { key : Key
    , subModel : SubModel
    , canvasDimension : Dimension
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
    | GotCanvasElement Url (Result Dom.Error Element)


canvasId =
    "canvas"


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key NoModel { width = 0, height = 0 }
    , Task.attempt (GotCanvasElement url) (Dom.getElement canvasId)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.subModel ) of
        ( GotCanvasElement url e, _ ) ->
            case e of
                Ok { element } ->
                    ( { model | canvasDimension = { width = element.width, height = element.height } }
                    , Nav.pushUrl model.key <| Url.toString url
                    )

                _ ->
                    ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.replaceUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( ChangedUrl url, _ ) ->
            let
                ( subModel, subCmd ) =
                    case Route.fromUrl url of
                        Just Route.Mandelbrot ->
                            Tuple.mapBoth
                                Mandelbrot
                                (Cmd.map GotMandelbrotMsg)
                                (Mandelbrot.init model.canvasDimension)

                        Just Route.Rotate ->
                            Tuple.mapBoth
                                Rotate
                                (Cmd.map GotRotate)
                                (Rotate.init model.canvasDimension)

                        Just Route.Motion ->
                            Tuple.mapBoth
                                Motion
                                (Cmd.map GotMotion)
                                (Motion.init model.canvasDimension)

                        Just Route.Sphere ->
                            Tuple.mapBoth
                                Sphere
                                (Cmd.map GotSphere)
                                (Sphere.init model.canvasDimension)

                        Nothing ->
                            ( NoModel, Cmd.none )
            in
            ( { model | subModel = subModel }, subCmd )

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
                    Html.map GotRotate <| Rotate.view m

                Motion m ->
                    Html.map GotMotion <| Motion.view m

                Sphere m ->
                    Html.map GotSphere <| Sphere.view m

                _ ->
                    div [] []

        body =
            main_
                []
                [ navView, div [ id canvasId ] [ right ] ]
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
