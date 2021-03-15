module Pages.Protected exposing (Model, Msg, Params, page)

import Api
import Api.User
import Browser.Navigation exposing (Key)
import Components.Popup as Popup exposing (Popup)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Br
import Element.Font as Font
import Element.Input as In
import Graphql.Document
import Graphql.Http
import Hasura.Scalar exposing (Uuid(..))
import Json.Decode as D
import Palette exposing (palette)
import Responsive exposing (Responsive, fontSize, frw, pad)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


page : Page Params Model Msg
page =
    Page.protectedApplication
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { r : Responsive
    , url : Url Params
    , key : Key
    , popup : Popup
    , jwt : Api.JWT
    , user : Api.User.User
    }


init : Api.JWT -> Api.User.User -> Shared.Model -> Url Params -> ( Model, Cmd Msg )
init jwt user shared url =
    ( Model shared.r url shared.key shared.popup jwt user
    , Cmd.none
    )



-- UPDATE


type Msg
    = Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    { shared | popup = model.popup }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( { model | r = shared.r, key = shared.key, popup = shared.popup }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Protected page"
    , body =
        [ column [ centerX, centerY, spacing 50 ]
            [ column [ spacing 10, centerX ]
                [ el [ centerX ] <| text model.user.id
                , el [ centerX ] <| text model.user.account.email
                , el [ centerX ] <| text <| String.join "," model.user.account.roles
                ]
            , ( case model.user.account.providerGoogle of
                Just provider ->
                    column [ spacing 10 ]
                        [ el [ centerX] <| text "Google access token"
                        , paragraph [ centerX, width <| px 500 ] [ text provider.accessToken ]
                        ]
                Nothing ->
                    text "Google not connected"
                )
            , ( case model.user.account.providerPipedrive of
                Just provider ->
                    column [ spacing 10 ]
                        [ el [ centerX] <| text "Pipedrive access token"
                        , paragraph [ centerX, width <| px 500 ] [ text provider.accessToken ]
                        ]
                Nothing ->
                    text "Pipedrive not connected"
                )
            ]
        ]
    }
