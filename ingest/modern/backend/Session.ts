import { IConnectedEventData, IAuthenticatedEventData } from "../frontend/IIngest"
import { ISession } from "./ISession";
import EventEmitter from "./util/EventEmitter.ts";
import { WebSocketProtocolStatusCode } from "./util/WebSocketUtil.ts";

import { IngestDetails
         , MessageDestination
         , DataObjectUpdateOperation} from "./signaling/types.ts";

import * as ClientMessages from "./signaling/clientMessages.ts";
import * as ServerMessages from "./signaling/serverMessages.ts";

enum SessionState {
  Opening = 1000,
  AwaitingInitialization = 2000,
  Connected = 3000,
  Authenticated = 4000,
  Negotiating = 5000,
  Stopping = 6000
}

enum MediaState {

}

const enum RTSStatusCode {
  IngestClosed = 4000,
}

const PING_INTERVAL_MS: number = 15000;

export default class Session extends EventEmitter implements ISession {
  private socketURL: string;
  private socket?: WebSocket = null;
  private state: SessionState = SessionState.Opening;
  private serverConfig?: any = null;
  private traceId?: string = null;
  private peer?: RTCPeerConnection = null;
  private localStream?: any = null;
  private serverIngestStopped: boolean = false;
  private peerConnectionStopped: boolean = false;

  constructor(socketURL: string) {
    super();
    this.socketURL = socketURL;
    this.createSocket();
    setInterval(() => this.pingSocket(), PING_INTERVAL_MS);
  }

  authenticate(username: string, password: string) {
    if (this.state != SessionState.Connected) {
      console.warn(`Attempt to authenticate whilst in invalid state ${this.state}`);
      return;
    }
    this.sendToSocket({ "type": "authenticate"
                        , "username": username
                        , "password": password
                      });
  }

  startIngest(stream) {
    if (this.state != SessionState.Authenticated) {
      console.warn(`Attempt to start ingest when not authenticated`);
      return;
    }
    this.localStream = stream;
    this.sendToSocket({ "type": "start-ingest" });
  }

  stopIngest() {
    if (this.state != SessionState.Negotiating) {
      console.warn(`Attempt to stop ingest when not negotiating`);
      return;
    }
    this.sendToSocket({ "type": "stop-ingest" });
    this.peer.close();
    this.state = SessionState.Stopping;
  }

  createSocket() {
    const maybePreviousSocket = this.socket;
    this.socket = new WebSocket(this.socketURL);
    this.socket.onopen = (event) => {
      this.handleSocketOpen(event);

      if (maybePreviousSocket !== null) {
        maybePreviousSocket.onopen = null;
        maybePreviousSocket.onclose = null;
        maybePreviousSocket.onerror = null;
        maybePreviousSocket.onmessage = null;
        maybePreviousSocket.close();
      }
    };
    this.socket.onclose = (event) => this.handleSocketClose(event);
    this.socket.onerror = (event) => this.handleSocketError(event);
    this.socket.onmessage = (event) => this.handleSocketMessage(event);
  }

  async beginNegotiation() {
    let peer = this.peer;
    let iceRestartRequired = false;

    peer = this.peer = new RTCPeerConnection(this.serverConfig);
    peer.onicecandidate = (event) => this.handlePeerICECandidate(event);
    peer.onicegatheringstatechange = (event) => this.handlePeerICEGatheringStateChange(event);
    peer.oniceconnectionstatechange = (event) => this.handlePeerICEConnectionStateChange(event);
    peer.onconnectionstatechange = (event) => this.handlePeerConnectionStateChange(event);
    peer.onsignalingstatechange = (event) => this.handlePeerSignalingStateChange(event);
    peer.ontrack = (event) => this.handlePeerTrack(event);
    this.localStream.getTracks().forEach((track) => {
      console.log("Adding track", track.label);
      peer.addTrack(track)
    });

    try {

      // NOTE: the offerToXXX properties are considered legacy, but the
      // RTCRtpTransceiver isn't yet widely supported
      const offer = await peer.createOffer({
        "iceRestart": iceRestartRequired,
        "offerToReceiveAudio": false,
        "offerToReceiveVideo": false,
      });
      console.debug("Local description obtained.");

      this.sendToSocket({
        "type": "sdp.offer",
        "offer": offer.sdp,
      });
      console.debug("Local description sent to server.");

      await peer.setLocalDescription(offer);
      console.debug("Local description applied.");

    }
    catch(e) {

      console.error("Something went less than spectacularly whilst configuring the peer.");
    }
  }

  handleSocketOpen(event) {
    console.log(`Opened WebSocket to ${this.socketURL}. Waiting for initialization message.`);
    this.state = SessionState.AwaitingInitialization;
  }

  // TODO: restart logic, it really depends on whether the media session is there too...
  handleSocketClose(event: CloseEvent) {
    console.warn(`The socket closed with code ${event.code} and reason '${event.reason}'`);
    switch (event.code) {
      case WebSocketProtocolStatusCode.NormalClosure:

        // This shouldn't really happen at all, we'd use our custom
        // IngestClosed message if the stream died, so reconnect.
        this.reconnectSocket();
        break;

      case WebSocketProtocolStatusCode.GoingAway:

        // We only send this if we're shutting down the ingest
        this.switchServer();
        break;

      case WebSocketProtocolStatusCode.Client_ConnectionClosedAbnormally:

        // Sent by a browser if the connection goes away without a close frame - just assume
        // the server went away in this case. If cowboy caught the error, we'd get
        // an UnexpectedConnection, which implies at the least that something more
        // severe went wrong
        this.switchServer();
        break;

      case WebSocketProtocolStatusCode.UnexpectedCondition:

        // Sent by cowboy if the socket handler crashes
        this.reconnectSocket();
        break;

      case WebSocketProtocolStatusCode.Client_TLSFailure:

        // Pretty serious stuff
        this.createDeferredLogEntry({
          message: "TLS failure.",
          socketURL: this.socketURL
        });
        this.switchServer();
        break;

      case RTSStatusCode.IngestClosed:
        // The ingest has been closed, no need to do anything further
        break;

      default:

        // Unknown problem, just switch server
        this.createDeferredLogEntry({
          message: "Unknown socket close status.",
          socketURL: this.socketURL,
          status: event.code,
          reason: event.reason
        });
        this.switchServer();
        break;
    }
  }

  handleSocketError(event) {
    console.error("The socket closed due to an error.", event);
  }

  handleSocketMessage(event) {
    switch (this.state) {
      case SessionState.AwaitingInitialization:
        {
          this.init_handleSocketMessage(event);
        }
        break;
      case SessionState.Connected:
        {
          this.connected_handleSocketMessage(event);
        }
        break;
      case SessionState.Authenticated:
        {
          this.authenticated_handleSocketMessage(event);
        }
        break;
      case SessionState.Negotiating:
        {
          this.negotiating_handleSocketMessage(event);
        }
        break;
      case SessionState.Stopping:
        {
          this.stopping_handleSocketMessage(event);
        }
        break;
    }
  }

  init_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);

    switch (message.type) {
      case "pong":
        break;

      case "init":
        {
          const connectedEventData: IConnectedEventData = {traceId: message.traceId}
          this.traceId = message.traceId;
          this.state = SessionState.Connected;
          console.log(`Websocket session connected with identifier ${message.traceId}, moved to state ${SessionState[this.state]}`);
          this.emit("connected", connectedEventData);
        }
        break;
      default:
        this.unexpectedMessage(message);
    }
  }

  connected_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);

    switch (message.type) {
      case "pong":
        break;

      case "authenticated":
        {
          const authenticatedEventData: IAuthenticatedEventData = {}
          this.state = SessionState.Authenticated;
          this.serverConfig = {
            iceTransportPolicy: "all",
            iceServers: message.thisIngest.iceServers,

            // We know we need two candidates - one for audio, one for video
            iceCandidatePoolSize: 2
          };
          console.log(`Websocket session authenticated`);
          this.emit("authenticated", authenticatedEventData);
        }
        break;
      default:
        this.unexpectedMessage(message);
    }
  }

  authenticated_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);

    switch (message.type) {
      case "pong":
        break;

      case "ingest-started":
        {
          console.log(`Ingest started, beginning negotiation`);
          this.state = SessionState.Negotiating;
          this.serverIngestStopped = false;
          this.peerConnectionStopped = false;
          this.beginNegotiation();
        }
        break;
      default:
        this.unexpectedMessage(message);
    }
  }

  async negotiating_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);

    switch (message.type) {
      case "pong":
        break;

      case "sdp.offer-response":
        {
          console.debug("Remote description obtained.");
          await this.peer.setRemoteDescription({ "sdp": message.response, "type": "answer" });
          console.debug("Remote description applied.");
        }
        break;

      case "ice.candidate":
        {
          console.debug("Remote ICE candidate obtained.", message);
          await this.peer.addIceCandidate(new RTCIceCandidate({
            sdpMLineIndex: message.index,
            candidate: message.candidate
          }));
          console.debug("Remote ICE candidate applied.");
        }
        break;

      case "dataobject.message":
        {
          console.debug(`Received message - from: ${message.sender}, body: ${message.msg}`);
        }
        break;
      case "dataobject.update-response":
        {
          console.debug(`Update response - senderRef: ${message.senderRef}, response: ${message.response}`);
        }
        break;
      case "dataobject.broadcast":
        {
          console.debug("Data Object.", message.object);
        }
        break;

      default:
        this.unexpectedMessage(message);
    }
  }

  stopping_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);

    switch (message.type) {
      case "pong":
        break;

      case "ingest-stopped":
        this.serverIngestStopped = true;
        this.maybeEmitStopped();
        break;

      default:
        this.unexpectedMessage(message);
    }
  }

  handlePeerICECandidate(event: RTCPeerConnectionIceEvent) {
    const candidate = event.candidate;

    if (candidate === null) {
      console.log(`Local ICE candidate gathering has completed.`);
      this.sendToSocket({
        "type": "ice.done",
      });
    }
    else {
      console.log(`Local ICE candidate received for ${candidate.sdpMLineIndex}: '${candidate.candidate}'`);
      this.sendToSocket({
        "type": "ice.candidate",
        "candidate": candidate.candidate,
        "index": candidate.sdpMLineIndex,
      });
    }
  }

  handlePeerICEGatheringStateChange(event) {
    console.debug(`ICE Gathering State changed to ${event.target.iceGatheringState}`);
  }

  handlePeerICEConnectionStateChange(event) {
    console.debug(`ICE Connection State changed to ${event.target.iceConnectionState}`);
  }

  handlePeerConnectionStateChange(event) {
    console.debug(`Connection State changed to ${event.target.connectionState}`);
    if (event.target.connectionState == "connected") {
      setTimeout(() => { this.reportStats() }, 1000);
      this.emit("ingest-active", {});
    }
  }

  handlePeerSignalingStateChange(event) {
    console.debug(`Signaling State changed to ${event.target.signalingState}`);

    if (event.target.signalingState == "closed") {
      this.peer.onicecandidate = null;
      this.peer.onicegatheringstatechange = null;
      this.peer.oniceconnectionstatechange = null;
      this.peer.onconnectionstatechange = null;
      this.peer.onsignalingstatechange = null;
      this.peer.ontrack = null;
      this.peer = null;
      this.peerConnectionStopped = true;
      this.maybeEmitStopped();
    }
  }

  handlePeerTrack(event) {
    console.debug("Got track", event.track);
  }

  maybeEmitStopped() {
    if (this.serverIngestStopped && this.peerConnectionStopped) {
      this.state = SessionState.Authenticated;
      this.emit("ingest-stopped", {});
    }
  }

  reportStats() {
    if (this.state == SessionState.Negotiating) {
      this.peer.getSenders().forEach((sender) => sender.getStats().then((stats) => this.emit("ingest-" + sender.track.kind + "-stats", stats)))
      setTimeout(() => { this.reportStats() }, 1000);
    }
  }

  reconnectSocket() {
    this.createSocket();
  }

  switchServer() {
  }

  pingSocket() {
    if (this.state === SessionState.Opening) {
      return;
    }

    if (this.state === SessionState.AwaitingInitialization) {
      return;
    }

    this.sendToSocket({
      "type": "ping"
    });
  }

  unexpectedMessage(message) {
    console.error(`Got unexpected message with type ${message.type} in state ${this.state}.`, message);
  }

  createDeferredLogEntry(data) {
  }

  sendToSocket(message: ClientMessages.Message) {
    try {
      this.socket.send(JSON.stringify(message));
    }
    catch (e) {

      // TODO: what happens when we exceed the buffer, the spec says an exception
      // but doesn't say which one
      switch (e.name) {
        case "InvalidStateError":
          console.error("Trying to send a message whilst the socket isn't open.");
          break;

        case "SyntaxError":
          // Unpaired surrogates according to the MDN docs...
          console.error("Trying to send a message containing a syntax error.");
          break;

        default:
          console.error("Unexpected error sending a message on the socket.");
          break;
      }
    }
  }
}
