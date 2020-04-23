import ISession from "./ISession";
import EventEmitter from "./util/EventEmitter.ts";
import { WebSocketProtocolStatusCode } from "./util/WebSocketUtil.ts";

import { IQualityConstraintConfiguration
         , MessageDestination
         , DataObjectUpdateOperation} from "./signaling/types.ts";

import * as ClientMessages from "./signaling/clientMessages.ts";
import * as ServerMessages from "./signaling/serverMessages.ts";

enum SessionState {
  Opening = 1000,
  AwaitingInitialization = 2000,
  Negotiating = 3000,
}

const enum RTSStatusCode {
  StreamFinished = 4000,
}

const PING_INTERVAL_MS: number = 15000;

export default class Session extends EventEmitter implements ISession {
  private socketURL: string;
  private socket?: WebSocket = null;
  private state: SessionState = SessionState.Opening;
  private traceId?: string = null;
  private serverConfig?: any = null;
  private peer?: RTCPeerConnection = null;

  // NOTE: Firefox currently doesn't support this
  private peerCanReconfigure = ("setConfiguration" in RTCPeerConnection.prototype);

  constructor(socketURL: string) {
    super();
    this.socketURL = socketURL;
    this.createSocket();
    setInterval(() => this.pingSocket(), PING_INTERVAL_MS);
  }

  createSocket() {

    // TODO: atomic switchover
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

      case RTSStatusCode.StreamFinished:

        // The stream has finished, no need to do anything further
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

      case "quality-change":
        {
          console.debug("The server has switched the stream variant.", message);
        }
        break;

      case "on-fi":
        {
          console.debug("Source encoder onFI.", message.timestamp, message.pts);
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

  unexpectedMessage(message) {
    console.error(`Got unexpected message with type ${message.type} in state ${this.state}.`, message);
  }

  setQualityConstraint(constraintConfiguration: IQualityConstraintConfiguration) {
      this.sendToSocket({
        "type": "set-quality-constraint-configuration",
        "configuration": constraintConfiguration
      });
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

  sendUpdate(operation: DataObjectUpdateOperation, senderRef: string) {
    this.sendToSocket({
      "type": "dataobject.update",
      "senderRef": senderRef,
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
        "offerToReceiveVideo": true,
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
