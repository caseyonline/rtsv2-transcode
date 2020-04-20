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
        this.emit("userMediaReady");
      })
      .catch((error) => {
        this.emit("userMediaFailed", error);
      });
  }

  connect() {
    const client = this.client = new RTCPeerConnection(this.options.serverConfig);
    client.onicecandidate = (event) => this.handleIceCandidateFromClient(event);
    client.oniceconnectionstatechange = (event) => this.handleIceConnectionChange(event);
    client.onaddstream = (event) => this.handleGotRemoteStream(event);

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
    this.emit("disconnected");
  }

  handleIceCandidateFromClient(event) {
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
    this.log.debug("Connection state change", event);
  }

  signal(payload) {
    this.signalling.sendRtc(JSON.stringify(payload));
  }

  handleSignal(encodedResponse) {
    const client = this.client;
    const response = JSON.parse(encodedResponse);

    if (response.sdp) {
      this.log.debug("received remote sdp:", response.sdp);

      const desc = {"type": "answer", "sdp": response.sdp};

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

  handleGotRemoteStream(event) {
    this.remoteStream = event.stream;
    this.mediaElement.srcObject = event.stream;
    this.log.debug("client received remote stream", event);
    this.emit("connected");
  }
}
