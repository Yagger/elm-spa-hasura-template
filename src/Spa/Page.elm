module Spa.Page exposing
    ( Page
    , static, sandbox, element, application
    , protectedApplication
    )

{-|

@docs Page
@docs static, sandbox, element, application
@docs Upgraded, Bundle, upgrade

-}

import Api
import Api.User
import Browser.Navigation as Nav exposing (Key)
import Shared exposing (Auth(..))
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Url exposing (Url)


type alias Page params model msg =
    { init : Shared.Model -> Url params -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Document msg
    , subscriptions : model -> Sub msg
    , save : model -> Shared.Model -> Shared.Model
    , load : Shared.Model -> model -> ( model, Cmd msg )
    }


static :
    { view : Url params -> Document msg
    }
    -> Page params (Url params) msg
static page =
    { init = \_ url -> ( url, Cmd.none )
    , update = \_ model -> ( model, Cmd.none )
    , view = page.view
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


sandbox :
    { init : Url params -> model
    , update : msg -> model -> model
    , view : model -> Document msg
    }
    -> Page params model msg
sandbox page =
    { init = \_ url -> ( page.init url, Cmd.none )
    , update = \msg model -> ( page.update msg model, Cmd.none )
    , view = page.view
    , subscriptions = \_ -> Sub.none
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


element :
    { init : Url params -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Document msg
    , subscriptions : model -> Sub msg
    }
    -> Page params model msg
element page =
    { init = \_ params -> page.init params
    , update = \msg model -> page.update msg model
    , view = page.view
    , subscriptions = page.subscriptions
    , save = always identity
    , load = always (identity >> ignoreEffect)
    }


application :
    { init : Shared.Model -> Url params -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Document msg
    , subscriptions : model -> Sub msg
    , save : model -> Shared.Model -> Shared.Model
    , load : Shared.Model -> model -> ( model, Cmd msg )
    }
    -> Page params model msg
application page =
    page


ignoreEffect : model -> ( model, Cmd msg )
ignoreEffect model =
    ( model, Cmd.none )


type alias ProtectedModel model params =
    { model
        | url : Url params
        , key : Key
        , jwt : Api.JWT
        , user : Api.User.User
    }


protectedApplication :
    { init : Api.JWT -> Api.User.User -> Shared.Model -> Url params -> ( ProtectedModel model params, Cmd msg )
    , update : msg -> ProtectedModel model params -> ( ProtectedModel model params, Cmd msg )
    , view : ProtectedModel model params -> Document msg
    , subscriptions : ProtectedModel model params -> Sub msg
    , save : ProtectedModel model params -> Shared.Model -> Shared.Model
    , load : Shared.Model -> ProtectedModel model params -> ( ProtectedModel model params, Cmd msg )
    }
    -> Page params (ProtectedModel model params) msg
protectedApplication page =
    { init =
        \shared params ->
            case shared.auth of
                Loading ->
                    ( page.init Api.emptyJWT Api.User.emptyUser shared params |> Tuple.first, Cmd.none )

                Anonymous ->
                    ( page.init Api.emptyJWT Api.User.emptyUser shared params |> Tuple.first, Nav.pushUrl params.key (Route.toString Route.Login) )

                UserLoading jwt ->
                    ( page.init jwt Api.User.emptyUser shared params |> Tuple.first, Cmd.none )

                Authorized jwt user ->
                    page.init jwt user shared params
    , update = page.update
    , view = page.view
    , subscriptions = page.subscriptions
    , save = page.save
    , load =
        \shared_ model_ ->
            case shared_.auth of
                Authorized jwt user ->
                    if model_.user == Api.User.emptyUser then
                        page.init jwt user shared_ model_.url

                    else
                        page.load shared_ model_

                Anonymous ->
                    ( model_, Nav.pushUrl model_.key (Route.toString Route.Login) )

                _ ->
                    page.load shared_ model_
    }
