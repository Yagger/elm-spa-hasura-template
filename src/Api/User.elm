module Api.User exposing (..)

-- import Hasura.Enum.Auth_roles_enum as ApiAuthRolesEnum

import Api
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hasura.Object
import Hasura.Object.Auth_account_providers
import Hasura.Object.Auth_account_roles as ApiAccountRoles
import Hasura.Object.Auth_accounts
import Hasura.Object.Users
import Hasura.Query
import Hasura.Scalar exposing (Citext(..), Uuid(..))


type alias User =
    { id : String
    , display_name : String
    , account : Account
    }


emptyUser : User
emptyUser =
    { id = ""
    , display_name = ""
    , account = emptyAccount
    }


userSelectionSet : SelectionSet User Hasura.Object.Users
userSelectionSet =
    SelectionSet.map3 User
        (SelectionSet.map (\(Uuid s) -> s) Hasura.Object.Users.id)
        (SelectionSet.withDefault "" Hasura.Object.Users.display_name)
        (SelectionSet.withDefault emptyAccount (Hasura.Object.Users.account accountSelectionSet))


type alias Account =
    { email : String
    , active : Bool
    , roles : List String
    , providerGoogle : Maybe AccountProvider
    , providerPipedrive : Maybe AccountProvider
    }


emptyAccount : Account
emptyAccount =
    { email = ""
    , active = False
    , roles = []
    , providerGoogle = Nothing
    , providerPipedrive = Nothing
    }


accountSelectionSet : SelectionSet Account Hasura.Object.Auth_accounts
accountSelectionSet =
    SelectionSet.map5 Account
        (SelectionSet.withDefault (Citext "") Hasura.Object.Auth_accounts.email
            |> SelectionSet.map (\(Citext s) -> s)
        )
        Hasura.Object.Auth_accounts.active
        -- ( Hasura.Object.Auth_accounts.account_roles identity accountRoleSelectionSet )
        (Hasura.Object.Auth_accounts.account_roles identity ApiAccountRoles.role)
        (Hasura.Object.Auth_accounts.account_providers identity accountProvidersSelectionSet
            |> SelectionSet.map (getProvider "google")
        )
        (Hasura.Object.Auth_accounts.account_providers identity accountProvidersSelectionSet
            |> SelectionSet.map (getProvider "pipedrive")
        )



-- accountRoleSelectionSet : SelectionSet String Hasura.Object.Auth_account_roles
-- accountRoleSelectionSet =
--     SelectionSet.map ApiAuthRolesEnum.toString ApiAccountRoles.role


type alias AccountProvider =
    { provider : String
    , providerId : String
    , accessToken : String
    , refreshToken : String
    }


accountProvidersSelectionSet : SelectionSet AccountProvider Hasura.Object.Auth_account_providers
accountProvidersSelectionSet =
    SelectionSet.map4 AccountProvider
        Hasura.Object.Auth_account_providers.auth_provider
        Hasura.Object.Auth_account_providers.auth_provider_unique_id
        (SelectionSet.withDefault "" Hasura.Object.Auth_account_providers.provider_access_token)
        (SelectionSet.withDefault "" Hasura.Object.Auth_account_providers.provider_refresh_token)


getProvider : String -> List AccountProvider -> Maybe AccountProvider
getProvider provider l =
    List.filter (\i -> i.provider == provider) l
        |> List.head


fetchUser : String -> String -> (Result (Graphql.Http.Error (Maybe User)) (Maybe User) -> msg) -> Cmd msg
fetchUser userID token onResponse =
    Api.query
        token
        (Hasura.Query.users_by_pk { id = Uuid userID } userSelectionSet)
        onResponse
