module Pages.Register exposing (Model, Msg, Params, page)

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Br
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as In
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Responsive exposing (Responsive, frw, pad, fontSize)
import Api
import Http
import Components.Popup as Popup exposing (Popup)
import Utils
import Browser.Navigation as Nav exposing (Key)


page : Page Params Model Msg
page =
    Page.application
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
    , key : Key
    , popup : Popup
    , auth : Shared.Auth
    , email : String
    , password : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( Model shared.r shared.key shared.popup shared.auth "" "", Cmd.none )



-- UPDATE


type Msg
    = InputEmail String
    | InputPassword String
    | Register
    | OnRegister ( Result Http.Error () )
    | OnLogin ( Result Http.Error Api.JWT )
    | ClosePopup


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail value ->
            ( { model | email = value }, Cmd.none )

        InputPassword value ->
            ( { model | password = value }, Cmd.none )
        
        Register ->
            ( model, Api.register { email = model.email, password = model.password } OnRegister )

        OnRegister result ->
            case result of
                Ok () ->
                    ( model, Api.login { email = model.email, password = model.password } OnLogin )
                Err error ->
                    ( { model | popup = Popup.HttpError <| Api.errorToString error }, Utils.delay 5000 ClosePopup )

        OnLogin result ->
            case result of
                Ok jwt ->
                    ( { model | auth = Shared.Authorized jwt ( Api.getUserIDFromJWT jwt ) }, Nav.replaceUrl model.key ( Route.toString Route.Top ) )
                Err error ->
                    ( { model | popup = Popup.HttpError <| Api.errorToString error }, Utils.delay 5000 ClosePopup )

        ClosePopup ->
            ( { model | popup = Popup.Closed }, Cmd.none )

save : Model -> Shared.Model -> Shared.Model
save model shared =
    { shared | popup = model.popup, auth = model.auth }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        r = model.r
    in
    { title = "Register"
    , body =
        [ column [ centerX, centerY, spacing <| frw r 30 ]
            [ el [ fontSize r 22 ] <| text "Please register using your email and password"
            , column [ spacing <| frw r 10 ]
                [ In.email [ width <| px <| frw r 300 ]
                    { onChange = InputEmail
                    , text = model.email
                    , placeholder = Nothing
                    , label = In.labelLeft
                        [ width <| px <| frw r 100
                        , Font.alignRight
                        , pad r 0 10 0 0
                        ] <| text "Email"
                    }
                , In.newPassword [ width <| px <| frw r 300 ]
                    { onChange = InputPassword
                    , text = model.password
                    , placeholder = Nothing
                    , label = In.labelLeft
                        [ width <| px <| frw r 100
                        , Font.alignRight
                        , pad r 0 10 0 0
                        ] <| text "Password"
                    , show = False
                    }
                ]
            , In.button [ centerX ] { onPress = Just Register, label = el
                [ Font.underline
                , fontSize r 22
                ] <| text "Register" }
            , row [ centerX ]
                [ text "Already have account? "
                , link [ Font.underline ] { url = Route.toString Route.Login, label = text "Login" }
                ]
            ]
        ]
    }