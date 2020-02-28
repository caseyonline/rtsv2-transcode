# Player/Server Interaction

## Scope
The details of the interaction between the player and the server are considered private and subject to change with
new versions of the player/SDK.

## Signaling Overview
Signaling between the player and the server occurs in two phases.

In the first phase, an HTTP request is made to work out to where to optimally open a WebSocket. This is described
in the OpenAPI file `discovery.yaml`.

In the second phase, the WebSocket is opened and signaling of the WebRTC begins.

WebSocket communication is by JSON over text frames. The format of which are described in the TypeScript files `signaling/serverMessages.ts` and
`signaling/clientMessages.ts`.

The WebSocket server may close the WebSocket at any time, in which case it will attempt to use an appropriate status code when it does
so. See https://tools.ietf.org/html/rfc6455#page-45 for information about status code. The status codes the server uses are described in
`websocketCodes.ts`.

The result of the second phase initialization might also be an application-level redirect if, for example, the initial
HTTP request made a decision based on out-of-date information, and the initial request was made to an inappropriate
server.

The moment the WebSocket is opened, the session is considered to be in an opening state, no message is required to be sent
from the player to the WebSocket.

The server will send either a `hello` message, or a `bye` message to complete the session-setup.

