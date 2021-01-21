var apolloClient = require("apollo-client")
var apolloLinkWS = require('apollo-link-ws')
var apolloCacheInMemory = require("apollo-cache-inmemory")
var gql = require('graphql-tag')

var GRAPHQL_URI = 'hasura.io/learn/graphql'

const getClient = (token) => {
    const wsLink = new apolloLinkWS.WebSocketLink({
        uri: `wss://${GRAPHQL_URI}`,
        options: {
            reconnect: true,
            connectionParams: {
                headers: {
                    Authorization: `Bearer ${ token }`
                }
            }
        }
    });

    const client = new apolloClient.ApolloClient({
        link: wsLink,
        cache: new apolloCacheInMemory.InMemoryCache({
            addTypename: true
        })
    });
    return client;
};

// Initial data passed to Elm (should match `Flags` defined in `Shared.elm`)
// https://guide.elm-lang.org/interop/flags.html
var flags = null

// Start our Elm application
var app = Elm.Main.init({ flags: flags })

// Ports go here
// https://guide.elm-lang.org/interop/ports.html

// app.ports.storeToken.subscribe(function(token) {
//     localStorage.setItem('token', token)
// })
// app.ports.removeTokenFromStarage.subscribe(function() {
//     localStorage.removeItem('token')
// })
// var token = localStorage.getItem('token')
// app.ports.gotStoredToken.send(token || "")

// app.ports.subscribeToUserEvents.subscribe(function(token, query) {
//     getClient(token).subscribe({
//         query: gql`${query}`,
//         variables: {}
//     }).subscribe({
//         next(resp) {
//             console.log('>>>>>', resp)
//             app.ports.onChangeInUserEvents.send(resp)
//         },
//         error(err) {
//             console.log('eeeee', err)
//             app.ports.onErrorInUserEvents.send(err)
//         }
//     });
// })