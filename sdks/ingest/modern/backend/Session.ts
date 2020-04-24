import { ISession
         , IConnectedEventData
         , IAuthenticatedEventData} from "./ISession";

import EventEmitter from "../../../shared/util/EventEmitter.ts";

import { WebSocketProtocolStatusCode } from "../../../shared/util/WebSocketUtil.ts";

import { IngestDetails
         , StreamIngestProtocol } from "../../../shared/llnw-types.ts"

import { MessageDestination
         , DataObjectUpdateOperation } from "../../../shared/data-object-types.ts";

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
  private requestedBitrate?: number = null;
  private serverIngestStopped: boolean = false;
  private peerConnectionStopped: boolean = false;

  constructor(socketURL: string) {
    super();
    this.socketURL = socketURL;
    this.createSocket();
    setInterval(() => this.pingSocket(), PING_INTERVAL_MS);
  }

  authenticate(username: string, password: string, protocol: StreamIngestProtocol) {
    if (this.state != SessionState.Connected) {
      console.warn(`Attempt to authenticate whilst in invalid state ${this.state}`);
      return;
    }
    this.sendToSocket({ "type": "authenticate"
                        , "username": username
                        , "password": password
                        , "protocol": protocol
                      });
  }

  startIngest(stream, bitrate) {
    if (this.state != SessionState.Authenticated) {
      console.warn(`Attempt to start ingest when not authenticated`);
      return;
    }
    this.localStream = stream;
    this.requestedBitrate = bitrate
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

  sendMessage(destination: MessageDestination, msg: string) {
    this.sendToSocket({
      "type": "dataobject.send-message",
      "destination": destination,
      "msg": msg
    });
  }

  sendUpdate(operation: DataObjectUpdateOperation, senderRef: string) {
    this.sendToSocket({
      "type": "dataobject.update",
      "senderRef": senderRef,
      "operation": operation
    })
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

      offer["sdp"] = this.setMediaBitrate(offer.sdp, "video", this.requestedBitrate);

      console.debug("Local description obtained.", offer.sdp);

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
    switch (this.state) {
      case SessionState.Opening:
        {
          // Nothing to do
        }
        break;
      case SessionState.AwaitingInitialization:
        {
          // Nothing to do
        }
        break;
      case SessionState.Connected:
        {
          // Nothing to do
        }
        break;
      case SessionState.Authenticated:
        {
          // Nothing to do
        }
        break;
      case SessionState.Negotiating:
        {
          this.serverIngestStopped = true;
          this.peer.close();
          this.clearPeerCallbacks();
        }
        break;
    }

    this.resetState();
    this.emit("reset", {});

    if (event.code == 1006) {
      setTimeout(() => {
        this.createSocket();
      }, 1000);
    }
    else {
      this.createSocket();
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
      case "dataobject.message":
        {
          this.emit("data-object-message", message);
        }
        break;

      case "dataobject.update-response":
        {
          this.emit("data-object-update-response", message);
        }
        break;

      case "dataobject.broadcast":
        {
          this.emit("data-object", message.object);
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
          var sdp = this.setMediaBitrate(message.response, "video", this.requestedBitrate);
          console.debug("Remote description obtained.", sdp);

          await this.peer.setRemoteDescription({ "sdp": sdp, "type": "answer" });
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
          this.emit("data-object-message", message);
        }
        break;

      case "dataobject.update-response":
        {
          this.emit("data-object-update-response", message);
        }
        break;

      case "dataobject.broadcast":
        {
          this.emit("data-object", message.object);
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
    switch (event.target.connectionState) {
      case "connected":
        {
          setTimeout(() => { this.reportStats() }, 1000);
          this.emit("ingest-active", {});
        }
        break;
      case "failed":
        {
          this.sendToSocket({ "type": "stop-ingest" });
          this.emit("ingest-stopped", {});
        }
        break;
    }
  }

  handlePeerSignalingStateChange(event) {
    console.debug(`Signaling State changed to ${event.target.signalingState}`);

    if (event.target.signalingState == "closed") {
      this.clearPeerCallbacks();
      this.peer = null;
      this.peerConnectionStopped = true;
      this.maybeEmitStopped();
    }
  }

  clearPeerCallbacks() {
      this.peer.onicecandidate = null;
      this.peer.onicegatheringstatechange = null;
      this.peer.oniceconnectionstatechange = null;
      this.peer.onconnectionstatechange = null;
      this.peer.onsignalingstatechange = null;
      this.peer.ontrack = null;
  }

  resetState() {
    this.socket = null;
    this.state = SessionState.Opening;
    this.serverConfig = null;
    this.traceId = null;
    this.peer = null;
    this.localStream = null;
    this.requestedBitrate = null;
    this.serverIngestStopped = false;
    this.peerConnectionStopped = false;
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

  setMediaBitrate(sdp, media, bitrate) {
    var lines = sdp.split("\n");
    var line = -1;
    for (var i = 0; i < lines.length; i++) {
      if (lines[i].indexOf("m="+media) === 0) {
        line = i;
        break;
      }
    }
    if (line === -1) {
      return sdp;
    }

    // Pass the m line
    line++;

    // Skip i and c lines
    while(lines[line].indexOf("i=") === 0 || lines[line].indexOf("c=") === 0) {
      line++;
    }

    var newLines
    // If we're on a b line, replace it
    if (lines[line].indexOf("b") === 0) {
      lines[line] = "b=AS:"+bitrate;
      newLines = lines.slice(0, line);
    }
    else {
      // Add a new b line
      newLines = lines.slice(0, line);
      newLines.push("b=AS:"+bitrate);
    }

    newLines.push("a=fmtp:102 x-google-start-bitrate=" + bitrate + "; x-google-max-bitrate=" + Math.trunc(bitrate * 1.1) + "; x-google-min-bitrate=" + Math.trunc(bitrate * 0.75));
    newLines = newLines.concat(lines.slice(line, lines.length));

    return newLines.join("\n");
  }

}
