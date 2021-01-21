module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    , Auth(..)
    )

import Api
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Components.MobileMenu as MobileMenu
import Components.Popup as Popup exposing (Popup)
import Element exposing (..)
import Element.Background as Bg
import Element.Font as Font
import Http
import Palette exposing (palette)
import Responsive exposing (Responsive, fontSize)
import Spa.Document exposing (Document)
import Task
import Time
import Url exposing (Url)
import Spa.Generated.Route as Route



-- INIT


type alias Flags =
    ()


type Auth
    = Anonymous
    | Authorized Api.JWT String


type alias Model =
    { url : Url
    , key : Key
    , r : Responsive
    , auth : Auth
    , popup : Popup
    , mobileMenu : Bool
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url
      , key = key
      , r = Responsive.make 0 0
      , auth = Anonymous
      , popup = Popup.Closed
      , mobileMenu = False
      }
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform GotViewport
        , Task.perform RefreshToken Time.now
        ]
    )



-- UPDATE


type Msg
    = GotViewport Browser.Dom.Viewport
    | OnResize Int Int
    | RefreshToken Time.Posix
    | TokenRefreshed (Result Http.Error Api.JWT)
    | ClosePopup
    | ShowMenu
    | HideMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport v ->
            ( { model | r = Responsive.make (floor v.viewport.width) (floor v.viewport.height) }, Cmd.none )

        OnResize width height ->
            let
                oldR =
                    model.r

                newR =
                    Responsive.make width height

                r =
                    case newR.device.class of
                        Desktop ->
                            newR

                        BigDesktop ->
                            newR

                        Tablet ->
                            -- Only adapt on orientation change, otherwise there will be problems with zooming on mibile devices.
                            if model.r.device.orientation /= newR.device.orientation then
                                newR

                            else
                                oldR

                        Phone ->
                            -- Only adapt on orientation change, otherwise there will be problems with zooming on mibile devices.
                            if model.r.device.orientation /= newR.device.orientation then
                                newR

                            else
                                oldR
            in
            ( { model | r = r }, Cmd.none )

        RefreshToken _ ->
            ( model, Api.refreshToken TokenRefreshed )

        TokenRefreshed result ->
            case result of
                Ok jwt ->
                    ( { model | auth = Authorized jwt ( Api.getUserIDFromJWT jwt ) }, Cmd.none )

                Err error ->
                    case error of
                        ( Http.BadStatus 401 ) ->
                            ( { model | auth = Anonymous } , Nav.replaceUrl model.key ( Route.toString Route.Login ) )
                        _ ->
                            ( { model | auth = Anonymous, popup = Popup.HttpError <| Api.errorToString error } , Nav.replaceUrl model.key ( Route.toString Route.Login ) )

        ClosePopup ->
            ( { model | popup = Popup.Closed }, Cmd.none )

        ShowMenu ->
            ( { model | mobileMenu = True }, Cmd.none )

        HideMenu ->
            ( { model | mobileMenu = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize OnResize
        , case model.auth of
            Authorized jwt userID ->
                Time.every ( toFloat jwt.jwt_expires_in ) RefreshToken
            _ ->
                Sub.none
        ]



-- VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        [ column
            -- main container
            [ width fill
            , height fill
            , fontSize model.r 18
            , Font.color palette.dark
            , Font.family
                [ Font.typeface "Noto Sans KR"
                , Font.sansSerif
                ]
            , Font.regular
            , Bg.color palette.white
            , inFront <| Popup.view model.r { popup = model.popup, onClose = toMsg ClosePopup }
            , inFront <|
                if model.mobileMenu then
                    MobileMenu.view model.r (toMsg HideMenu)

                else
                    none
            ]
            [ case model.r.device.class of
                Phone ->
                    mobileHeader model toMsg

                Tablet ->
                    mobileHeader model toMsg

                Desktop ->
                    header model toMsg

                BigDesktop ->
                    header model toMsg
            , column [ width fill, height fill ] page.body
            , case model.r.device.class of
                Phone ->
                    footer model toMsg

                Tablet ->
                    footer model toMsg

                Desktop ->
                    footer model toMsg

                BigDesktop ->
                    footer model toMsg
            ]
        ]
    }


header : Model -> (Msg -> msg) -> Element msg
header model toMsg =
    let
        r =
            model.r
    in
    case model.auth of
        Anonymous ->
            none
        _ ->
            row []
                [ text "Header"
                ]


mobileHeader : Model -> (Msg -> msg) -> Element msg
mobileHeader model toMsg =
    let
        r =
            model.r
    in
    row []
        [ MobileMenu.button r (toMsg ShowMenu)
        ]


footer : Model -> (Msg -> msg) -> Element msg
footer model toMsg =
    let
        r =
            model.r
    in
    row []
        [ none
        ]
