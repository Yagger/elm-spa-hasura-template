module Api.User exposing (..)


import Graphql.Http
import Hasura.Query
import Hasura.Object
import Hasura.Object.Users
import Hasura.Object.Auth_accounts
import Hasura.Object.Auth_account_roles as ApiAccountRoles
import Hasura.Enum.Auth_roles_enum as ApiAuthRolesEnum
import Hasura.Scalar exposing (Citext(..), Uuid(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Api


type alias User =
    { id : Uuid
    , display_name : String
    , account : Account
    }
userSelectionSet : SelectionSet User Hasura.Object.Users
userSelectionSet =
    SelectionSet.map3 User
        Hasura.Object.Users.id
        ( SelectionSet.withDefault "" Hasura.Object.Users.display_name )
        ( Hasura.Object.Users.account accountSelectionSet
            |> SelectionSet.withDefault emptyAccount
        )

type alias Account =
    { email : String
    , active : Bool
    , roles : List String
    }
accountSelectionSet : SelectionSet Account Hasura.Object.Auth_accounts
accountSelectionSet =
    SelectionSet.map3 Account
        ( SelectionSet.withDefault ( Citext "" ) Hasura.Object.Auth_accounts.email
            |> SelectionSet.map (\(Citext s) -> s)
        )
        Hasura.Object.Auth_accounts.active
        ( Hasura.Object.Auth_accounts.account_roles identity accountRoleSelectionSet
            |> SelectionSet.map ( List.map .role )
        )

type alias AccountRole =
    { role : String
    }
accountRoleSelectionSet : SelectionSet AccountRole Hasura.Object.Auth_account_roles
accountRoleSelectionSet =
    SelectionSet.map AccountRole
        ( SelectionSet.map ApiAuthRolesEnum.toString ApiAccountRoles.role )

emptyUser : User
emptyUser =
    { id = Uuid ""
    , display_name = ""
    , account = emptyAccount
    }
emptyAccount : Account
emptyAccount =
    { email = ""
    , active = False
    , roles = []
    }

fetchUser : String -> String -> ( Result ( Graphql.Http.Error  User ) User -> msg) -> Cmd msg
fetchUser userID token onResponse =
    Api.query
        token
        ( Hasura.Query.users_by_pk { id = Uuid userID } userSelectionSet
            |> SelectionSet.map ( Maybe.withDefault emptyUser )
        )
        onResponse
