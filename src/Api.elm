module Api exposing (..)

import Array
import Base64
import Conf
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)
import Http exposing (expectWhatever)
import Json.Decode as D
import Json.Encode as E
import Url
import Url.Builder


type alias JWT =
    { jwt_token : String
    , jwt_expires_in : Int
    }


emptyJWT : JWT
emptyJWT =
    { jwt_token = ""
    , jwt_expires_in = 0
    }


getUserIDFromJWT : JWT -> String
getUserIDFromJWT jwt =
    String.split "." jwt.jwt_token
        |> Array.fromList
        |> Array.get 1
        |> Maybe.withDefault ""
        |> Base64.decode
        |> Result.withDefault ""
        |> D.decodeString (D.at [ "https://hasura.io/jwt/claims", "x-hasura-user-id" ] D.string)
        |> Result.withDefault ""


type alias LoginForm =
    { email : String
    , password : String
    }


loginFormEncode : LoginForm -> E.Value
loginFormEncode form =
    E.object
        [ ( "email", E.string form.email )
        , ( "password", E.string form.password )
        ]


type alias ErrorResponse =
    { statusCode : Int
    , error : String
    , message : String
    }


errorResponseDecoder : D.Decoder ErrorResponse
errorResponseDecoder =
    D.map3 ErrorResponse
        (D.field "statusCode" D.int)
        (D.field "error" D.string)
        (D.field "message" D.string)


jwtDecoder : D.Decoder JWT
jwtDecoder =
    D.map2 JWT
        (D.field "jwt_token" D.string)
        (D.field "jwt_expires_in" D.int)


refreshToken : (Result Http.Error JWT -> msg) -> Cmd msg
refreshToken onResponse =
    Http.riskyRequest
        { method = "GET"
        , url = Conf.authApiUrl ++ "/auth/token/refresh"
        , body = Http.emptyBody
        , expect = Http.expectJson onResponse jwtDecoder
        , timeout = Nothing
        , headers = []
        , tracker = Nothing
        }


register : LoginForm -> (Result Http.Error () -> msg) -> Cmd msg
register form onResponse =
    Http.post
        { url = Conf.authApiUrl ++ "/auth/register"
        , body = Http.jsonBody <| loginFormEncode form
        , expect = expectWhatever onResponse errorResponseDecoder
        }


login : LoginForm -> (Result Http.Error JWT -> msg) -> Cmd msg
login form onResponse =
    Http.riskyRequest
        { method = "POST"
        , url = Conf.authApiUrl ++ "/auth/login"
        , body = Http.jsonBody <| loginFormEncode form
        , expect = expectJson onResponse jwtDecoder errorResponseDecoder
        , timeout = Nothing
        , headers = []
        , tracker = Nothing
        }


logout : (Result Http.Error () -> msg) -> Cmd msg
logout onResponse =
    Http.riskyRequest
        { method = "POST"
        , url = Conf.authApiUrl ++ "/auth/logout"
        , body = Http.emptyBody
        , expect = expectWhatever onResponse errorResponseDecoder
        , timeout = Nothing
        , headers = []
        , tracker = Nothing
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody message ->
            message


graphqlErrorToString : Graphql.Http.Error parsedData -> String
graphqlErrorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map (\i -> i.message)
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    "Bad URL: " ++ url

                Graphql.Http.Timeout ->
                    "Timeout"

                Graphql.Http.NetworkError ->
                    "Network error"

                Graphql.Http.BadStatus _ status ->
                    "Bad status: " ++ status

                Graphql.Http.BadPayload s ->
                    "Bad payload"


expectJson : (Result Http.Error a -> msg) -> D.Decoder a -> D.Decoder ErrorResponse -> Http.Expect msg
expectJson toMsg successDecoder failureDecoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                        case D.decodeString failureDecoder body of
                            Ok value ->
                                Err (Http.BadBody value.message)

                            Err err ->
                                Err (Http.BadBody (D.errorToString err))

                    else
                        Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString successDecoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))


expectWhatever : (Result Http.Error () -> msg) -> D.Decoder ErrorResponse -> Http.Expect msg
expectWhatever toMsg failureDecoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                        case D.decodeString failureDecoder body of
                            Ok value ->
                                Err (Http.BadBody value.message)

                            Err err ->
                                Err (Http.BadBody (D.errorToString err))

                    else
                        Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ _ ->
                    Ok ()


query :
    String
    -> SelectionSet decodesTo Graphql.Operation.RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
query token selectionSet onResponse =
    selectionSet
        |> Graphql.Http.queryRequest Conf.graphqlUrl
        |> Graphql.Http.withHeader "authorization" ("Bearer " ++ token)
        |> Graphql.Http.send onResponse


mutate :
    String
    -> SelectionSet decodesTo Graphql.Operation.RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
mutate token selectionSet onResponse =
    selectionSet
        |> Graphql.Http.mutationRequest Conf.graphqlUrl
        |> Graphql.Http.withHeader "authorization" ("Bearer " ++ token)
        |> Graphql.Http.send onResponse


oauthPrividerLink : String -> String
oauthPrividerLink provider =
    Url.Builder.crossOrigin Conf.authApiUrl [ "auth", "providers", provider ] []
