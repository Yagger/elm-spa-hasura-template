module Pages.Callback exposing (..)

import Api
import Api.User
import Browser.Navigation as Nav exposing (Key)
import Components.Popup as Popup exposing (Popup)
import Responsive exposing (Responsive)
import Shared exposing (Auth(..))
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Spa.Generated.Route as Route


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
    , Nav.replaceUrl shared.key ( Route.toString Route.Protected )
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
    , body = []
    }
