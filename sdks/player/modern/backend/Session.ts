import ISession from "./ISession";

import EventEmitter from "../../../shared/util/EventEmitter";

import { WebSocketProtocolStatusCode } from "../../../shared/util/WebSocketUtil";

import { IQualityConstraintConfiguration } from "./signaling/types";

import { MessageDestination
         , DataObjectUpdateOperation} from "../../../shared/data-object-types";

import * as ClientMessages from "./signaling/clientMessages";
import * as ServerMessages from "./signaling/serverMessages";

enum SessionState {
  Opening = 1000,
  AwaitingInitialization = 2000,
  Negotiating = 3000,
}

const PING_INTERVAL_MS: number = 15000;

export default class Session extends EventEmitter implements ISession {
  private socketURL: string;
  private socket?: WebSocket = null;
  private state: SessionState = SessionState.Opening;
  private traceId?: string = null;
  private serverConfig?: any = null;
  private peer?: RTCPeerConnection = null;
  private cookie?: string = null;
  private validationURL: string;
  private audioOnly: boolean = true;

  // NOTE: Firefox currently doesn't support this
  private peerCanReconfigure = ("setConfiguration" in RTCPeerConnection.prototype);

  constructor(socketURL: string, validationURL: string) {
    super();
    this.socketURL = socketURL;
    this.validationURL = validationURL;
    this.createSocket();
    setInterval(() => this.pingSocket(), PING_INTERVAL_MS);
  }

  createSocket() {

    // TODO: atomic switchover
    const maybePreviousSocket = this.socket;
    this.socket = new WebSocket(`${this.socketURL}?validation_url=${this.validationURL}`);
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

  stop() {
    this.socket.onopen = null;
    this.socket.onclose = null;
    this.socket.onerror = null;
    this.socket.onmessage = null;
    this.peer.onicecandidate = null;
    this.peer.onicegatheringstatechange = null;
    this.peer.oniceconnectionstatechange = null;
    this.peer.onconnectionstatechange = null;
    this.peer.onsignalingstatechange = null;
    this.peer.ontrack = null;
    this.socket.close();
    this.peer.close();
  }

  pingSocket() {
    if (this.state === SessionState.Opening) {
      return;
    }

    if (this.state === SessionState.AwaitingInitialization) {
      return;
    }

    this.sendToSocket({
      "type": "ping",
      validationCookie: this.cookie
    });
  }

  setQualityConstraints(constraints) {}

  handleSocketOpen(event) {
    console.log(`Opened WebSocket to ${this.socketURL}. Waiting for initialization message.`);
    this.state = SessionState.AwaitingInitialization;
  }

  reconnectSocket() {
    this.createSocket();
  }

  switchServer() {
  }

  createDeferredLogEntry(data) {
  }

  // TODO: restart logic, it really depends on whether the media session is there too...
  handleSocketClose(event: CloseEvent) {
    console.warn(`The socket closed with code ${event.code} and reason '${event.reason}'`);
    switch (event.code) {
      case WebSocketProtocolStatusCode.NormalClosure:

        // This shouldn't really happen at all, we'd use our custom
        // StreamFinished message if the stream died, so reconnect.
        this.reconnectSocket();
        break;

      case WebSocketProtocolStatusCode.GoingAway:

        // We only send this if we're shutting down the edge
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
      case SessionState.Negotiating:
        {
          this.negotiating_handleSocketMessage(event);
        }
        break;
    }
  }

  init_handleSocketMessage(event) {
    const message = <ServerMessages.Message> JSON.parse(event.data);
    switch (message.type) {
      case "init":
        {
          const thisEdge = message.thisEdge;

          this.state = SessionState.Negotiating;
          this.traceId = message.traceId;
          this.serverConfig = {
            iceTransportPolicy: "all",
            iceServers: thisEdge.iceServers,

            // We know we need two candidates - one for audio, one for video
            iceCandidatePoolSize: 2
          };
          this.cookie = message.validationCookie;
          this.audioOnly = message.audioOnly;

          console.log(`Initialized Session with identifier ${message.traceId}, moved to state ${SessionState[this.state]} (${this.state}). Final endpoint: ${thisEdge.socketURL}`, thisEdge);

          this.beginNegotiation();
        }
        break;

      case "bye":
        {
          if (message.otherEdges.length > 0) {
            this.socketURL = message.otherEdges[0].socketURL;
            console.log(`Rejected by server, we're being redirected to ${this.socketURL}.`);
            this.createSocket();
          }
          else {
            console.log(`Rejected by server, no alternatives were provided.`);
          }
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
        {
          this.cookie = message.validationCookie;
        }
        break;

      case "bye":
        {
          if (message.otherEdges.length > 0) {
            this.socketURL = message.otherEdges[0].socketURL;
            console.log(`Rejected by server, we're being redirected to ${this.socketURL}.`);
            this.createSocket();
          }
          else {
            console.log(`Rejected by server, no alternatives were provided.`);
          }
        }
        break;

      case "sdp.offer-response":
        {
          console.debug("Remote description obtained.", message.response);
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

      case "quality-change":
        {
          this.emit("quality-change", message);
        }
        break;

      case "on-fi":
        {
          this.emit("on-fi", message);
        }
        break;

      case "active-profiles":
        {
          this.emit("active-profiles", message.activeProfiles);
        }
        break;

      case "dataobject.message":
        {
          this.emit("data-object-message", message);
        }
        break;

      case "dataobject.message-failure":
        {
          this.emit("data-object-message-failure", message);
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

      case "time-zero":
        {
          this.emit("time-zero", message);
        }
        break;

      case "bye":
        {
          if (message.otherEdges.length > 0) {
            this.socketURL = message.otherEdges[0].socketURL;
            console.log(`Rejected by server, we're being redirected to ${this.socketURL}.`);
            this.createSocket();
          }
          else {
            console.log(`Rejected by server, no alternatives were provided.`);
          }
        }
        break;

      default:
        this.unexpectedMessage(message);
    }
  }

  unexpectedMessage(message) {
    console.error(`Got unexpected message with type ${message.type} in state ${this.state}.`, message);
  }

  setQuality(variant: string) {
      this.sendToSocket({
        "type": "set-quality",
        "variant": variant
      });
  }

  setQualityConstraint(constraintConfiguration: IQualityConstraintConfiguration) {
      this.sendToSocket({
        "type": "set-quality-constraint-configuration",
        "configuration": constraintConfiguration
      });
  }

  async reportStats() {
    if (this.state == SessionState.Negotiating) {
      this.peer.getReceivers().forEach(async (receiver) => {
        const stats = await receiver.getStats();
        this.emit("playback-" + receiver.track.kind + "-stats", stats);

        const syncSources = receiver.getSynchronizationSources();
        this.emit("playback-" + receiver.track.kind + "-sync-sources", syncSources);
      });

      setTimeout(() => { this.reportStats() }, 1000);
    }
  }

  requestMigrate(socketURL: string) {
    console.debug(`Attempting migration to ${socketURL}`);
    this.socketURL = socketURL;
    this.createSocket();
  }

  sendMessage(destination: MessageDestination, msg: string) {
    this.sendToSocket({
      "type": "dataobject.send-message",
      "destination": destination,
      "msg": msg
    });
  }

  sendUpdate(operation: DataObjectUpdateOperation, requestResponseCorrelationId: string) {
    this.sendToSocket({
      "type": "dataobject.update",
      "requestResponseCorrelationId": requestResponseCorrelationId,
      "operation": operation
    })
  }

  async beginNegotiation() {
    let peer = this.peer;
    let iceRestartRequired = false;

    if (peer !== null && this.peerCanReconfigure) {
      console.debug("Reconfiguring existing peer.");
      peer.setConfiguration(this.serverConfig);
      iceRestartRequired = true;
    }
    else {
      peer = this.peer = new RTCPeerConnection(this.serverConfig);
      peer.onicecandidate = (event) => this.handlePeerICECandidate(event);
      peer.onicegatheringstatechange = (event) => this.handlePeerICEGatheringStateChange(event);
      peer.oniceconnectionstatechange = (event) => this.handlePeerICEConnectionStateChange(event);
      peer.onconnectionstatechange = (event) => this.handlePeerConnectionStateChange(event);
      peer.onsignalingstatechange = (event) => this.handlePeerSignalingStateChange(event);
      peer.ontrack = (event) => this.handlePeerTrack(event);
    }

    try {

      // NOTE: the offerToXXX properties are considered legacy, but the
      // RTCRtpTransceiver isn't yet widely supported
      const offer = await peer.createOffer({
        "iceRestart": iceRestartRequired,
        "offerToReceiveAudio": true,
        "offerToReceiveVideo": !this.audioOnly,
      });
      console.debug("Local description obtained.", offer);

      this.sendToSocket({
        "type": "sdp.offer",
        "offer": offer.sdp
      });
      console.debug("Local description sent to server.");



      await peer.setLocalDescription(offer);
      console.debug("Local description applied.");

    }
    catch(e) {

      console.error("Something went less than spectacularly whilst configuring the peer.");
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
          this.emit("playback-active", {});
        }
        break;
    }
  }

  handlePeerSignalingStateChange(event) {
    console.debug(`Signaling State changed to ${event.target.signalingState}`);
  }

  handlePeerTrack(event) {
    console.debug("Got track", event.track);
    this.emit("stream", event.streams[0]);
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
