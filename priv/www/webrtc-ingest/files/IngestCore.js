"use strict";

import EventEmitter from "./EventEmitter.js";

function normalizeOptions(options) {
  let finalOptions = Object.assign({}, options);

  finalOptions.sendAudio = finalOptions.sendAudio || false;
  finalOptions.receiveAudio = finalOptions.receiveAudio || false;
  finalOptions.sendVideo = finalOptions.sendVideo || false;
  finalOptions.receiveVideo = finalOptions.receiveVideo || false;
  finalOptions.serverConfig = finalOptions.serverConfig || defaultServerConfig();

  return finalOptions;
}

function defaultServerConfig() {
  return {
    iceServers: [
      {
        urls: ["stun:stun1.l.google.com:19302"]
      }
    ]
  };
}

export default class RealTimeCore extends EventEmitter {
  constructor(mediaElement, log, signalling, options) {
    super();

    this.mediaElement = mediaElement;
    this.log = log;
    this.signalling = signalling;
    this.options = normalizeOptions(options);
    this.client = null;

    this.signalling.on("rtc", (message) => this.handleSignal(message));
  }

  getMediaElement() {
    return this.mediaElement;
  }

  requestUserMedia() {
    this.emit("userMediaRequesting");

    var sendAudio = this.options.sendAudio;
    var sendVideo = this.options.sendVideo;
    var sendAnything = sendAudio || sendVideo;

    if (!sendAnything) {
      this.emit("userMediaReady");
      return;
    }

    navigator.mediaDevices.getUserMedia({audio: sendAudio, video: sendVideo})
      .then((stream) => {
        this.localStream = stream;
        this.emit("userMediaReady", stream);
      })
      .catch((error) => {
        this.emit("userMediaFailed", error);
      });
  }

  connect() {
    const client = this.client = new RTCPeerConnection(this.options.serverConfig);

    client.onicecandidate = (event) => this.handleIceCandidateFromClient(event);
    client.oniceconnectionstatechange = (event) => this.handleIceConnectionChange(event);
    client.onconnectionstatechange = (event) => this.handleConnectionStateChange(event);


    if (this.localStream != null) {
      const audioTracks = this.localStream.getAudioTracks();

      client.addStream(this.localStream);
      this.log.debug("using audio device", audioTracks[0].label);
    }

    const offerOptions = {
      offerToReceiveAudio: this.options.receiveAudio ? 1 : 0,
      offerToReceiveVideo: this.options.receiveVideo ? 1 : 0
    };

    client.createOffer(
      offerOptions
    ).then(
      (desc) => {
        desc["sdp"] = this.setMediaBitrate(desc.sdp, "video", this.options.videoBitrate);
        this.log.debug("received local description, applying", desc);
        client.setLocalDescription(desc).then(
          () => {
            this.log.debug("local description applied, sending to server");
            this.signal(desc);
          },
          (error) => {
            this.emit("connectFailed", error);
            this.log.error("local description failed to apply", error);
          }
        );
      },
      (error) => {
        this.emit("connectFailed", error);
        this.log.error("failed to create session description", error.toString());
      }
    );
  }

  disconnect() {
    this.client.close();
    this.client = null;
    this.localStream.getTracks().forEach(function(track) {
      track.stop();
    });
    this.emit("disconnected");
  }

  handleIceCandidateFromClient(event) {
    if (this.client == null) return;
    if (event.candidate !== null) {
      this.log.debug("browser ICE candidate", event.candidate);
      this.signal(event.candidate);
    }
    else {
      this.log.debug("browser ICE candidate gathering complete");
      this.signal({"event": "candidateGatheringComplete"});
    }
  }

  handleIceConnectionChange(event) {
    if (this.client == null) return;
    this.log.debug("ICE Connection state change", event);
  }

  handleConnectionStateChange(event) {
    if (this.client == null) return;
    if (this.client.connectionState == "connected") {
      this.emit("connected");
    }
  }

  signal(payload) {
    this.signalling.sendRtc(JSON.stringify(payload));
  }

  handleSignal(encodedResponse) {
    if (this.client == null) return;
    const client = this.client;
    const response = JSON.parse(encodedResponse);

    if (response.sdp) {
      this.log.debug("received remote sdp:", response.sdp);

      const sdp2 = this.setMediaBitrate(response.sdp, "video", this.options.videoBitrate);
      const desc = {"type": "answer", "sdp": sdp2};

      client.setRemoteDescription(desc).then(
        () => {
          this.log.debug("setRemoteDescription complete");
        },
        (error) => {
          this.log.error("Failed to set remote description", error);
        }
      );
    }
    else if (response.server_ice_candidate) {
      client.addIceCandidate(new RTCIceCandidate(response.server_ice_candidate)).then(
        () => {
          this.log.debug("added server ice candidate successfully", response.server_ice_candidate);
        },
        (error) => {
          this.log.error("failed to add server ice candidate", error, response.server_ice_candidate);
        }
      );
    }
    else {
      this.log.warning("unknown RTC signal from server", response);
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
