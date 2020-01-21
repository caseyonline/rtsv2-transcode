const PING_INTERVAL_MS = 5000;
const SCHEDULE_PING_YES = true;
const SCHEDULE_PING_NO = false;

function wsPath(path) {
  const proto = (window.location.protocol === "https:" ? "wss:" : "ws:");
  return proto + window.location.host + window.location.pathname + path;
}

function httpPath(path) {
  return window.location.protocol + "//" + window.location.host + window.location.pathname + path;
}

export class Session extends EventTarget {
  constructor(rootPath, video) {
    super();
    this._httpBase = httpPath(rootPath);
    this._wsBase = wsPath(rootPath);
    this._video = video;

    this._signalSocket = null;
    this._pingTimerId = null;
    this._connection = null;
    this._remoteStream = null;
  }

  start() {
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = () => {
      if (xmlHttp.readyState == 4 && xmlHttp.status == 200) {
        var response = JSON.parse(xmlHttp.responseText);
        this._sessionId = response.token;
        const signalSocket = this._signalSocket = new WebSocket(this._wsBase + "/" + response.path);

        signalSocket.onopen = (event) => {
          this.dispatchEvent(new Event("ready"));
        };

        signalSocket.onmessage = (event) => {

          var response = JSON.parse(event.data);

          switch (response.type) {
          case "rtc":
            this._handleRTC(response.data);
            break;
          case "join":
            this._handleJoin(response.data);
            this._schedulePing();
            break;
          case "pong":
            break;
          default:
            const eventToRaise = new Event("unknownmessage");
            eventToRaise.message = response;
            this.dispatchEvent(eventToRaise);
          }
        }
      }
    }
    xmlHttp.open( "GET", this._httpBase + "/session?use_turn=0", true );
    xmlHttp.send( null );
  }

  stop() {
  }

  _sendRTCMessage(data) {
    this._sendMessage("rtc", JSON.stringify(data), SCHEDULE_PING_YES);
  }

  _sendMessage(type, data, schedulePing) {
    var toSend = { "type": type, "data": data };
    this._signalSocket.send(JSON.stringify(toSend));

    if (schedulePing) {
      this._schedulePing();
    }
  }

  _schedulePing() {
    this._cancelScheduledPing();
    this._pingTimerId = setTimeout(() => this._sendPing(), PING_INTERVAL_MS);
  }

  _cancelScheduledPing() {
    if ( this._pingTimerId === null ) {
      return;
    }

    clearTimeout(this._pingTimerId);
    this._pingTimerId = null;
  }

  _sendPing() {
    this._sendMessage("ping", undefined, SCHEDULE_PING_YES);
  }

  async _handleJoin(sessionId) {
    console.debug(`Connected with sesion id ${sessionId}.`);

    const turnServers = null;

    const config =  {
      iceServers: Array.prototype.concat(
        [
          {
            urls: ["stun:stun1.l.google.com:19302"]
          }
        ],
        turnServers ? turnServers : []
      )
    };

    const connection = this._connection = new RTCPeerConnection(config);
    connection.onicecandidate = (event) => this._handleBrowserIceCandidate(event);
    connection.oniceconnectionstatechange = (event) => this._handleBrowserIceConnectionChange(event);
    connection.onaddstream = (event) => this._handleGotRemoteStream(event);

    try {
      const offerOptions = {
        offerToReceiveAudio: 1,
        offerToReceiveVideo: 1,
      };

      const desc = await connection.createOffer(offerOptions);

      try {
        await connection.setLocalDescription(desc);
        console.debug("Local description applied, sending to server, SDP:", desc.sdp);
        this._sendRTCMessage(desc);
      }
      catch (error) {
        console.error("local description failed to apply with error:", error, "SDP:", desc.sdp);
      }
    }
    catch (error) {
      console.error("Failed to create session description", error.toString());
    }
  }

  async _handleRTC(encodedResponse) {
    const connection = this._connection;
    const response = JSON.parse(encodedResponse);

    if (response.sdp) {
      const desc = {"type": "answer", "sdp": response.sdp};

      try {
        await connection.setRemoteDescription(desc);
        console.debug("setRemoteDescription complete for:", response.sdp);
      }
      catch (error) {
        console.error("Failed to set remote description with error:", error, "SDP:", response.sdp);
      }
    }
    else if (response.server_ice_candidate) {
      try {
        await connection.addIceCandidate(new RTCIceCandidate(response.server_ice_candidate));
        console.debug("Added server ice candidate successfully", response.server_ice_candidate);
      }
      catch (error) {
        console.error("Failed to add server ice candidate", error, response.server_ice_candidate);
      };
    }
    else {
      console.warn("Unknown RTC signal from server", response);
    }
  }

  _handleBrowserIceCandidate(event) {
    if (event.candidate !== null) {
      const candidate = event.candidate;
      console.debug("Browser ICE candidate with media id", candidate.sdpMid, "and media line index", candidate.sdpMLineIndex, ":", candidate.candidate);
      this._sendRTCMessage(event.candidate);
    }
    else {
      console.debug("Browser ICE candidate gathering complete.");
      this._sendRTCMessage({"event": "candidateGatheringComplete"});
    }
  }

  _handleBrowserIceConnectionChange(event) {
    console.debug("Connection state changed to", event.target.signalingState);
  }

  _handleGotRemoteStream(event) {
    this._remoteStream = event.stream;
    this._video.srcObject = event.stream;
    this._video.play();
    console.debug("Client received remote stream", event);
  }
}
