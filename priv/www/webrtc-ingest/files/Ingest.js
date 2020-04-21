"use strict";

import EventEmitter from "./EventEmitter.js";
import IngestCore from "./IngestCore.js";
import Log from "./Log.js";
import SignalingChannel from "./IngestSignaling.js";

export default class Ingest extends EventEmitter {
  constructor(signalingUrl) {
    super()

    this.log = new Log("RTSV2 WebRTC Ingest: ");
    this.signaling = null;
    this.rtc = null;
    this.config = null;

    this.log.info("Loading...");

    this.signaling = new SignalingChannel(signalingUrl);
    this.signaling.on("unknown", (message) => this.log.debug("Unknown signaling channel message", message));
    this.signaling.on("joined", (sessionId) => this._handleJoin(sessionId));
  }

  start(username, password, bitrate) {
    this.emit("starting");
    this.signaling.start(username, password);
    this.bitrate = bitrate;
  }

  call() {
    this.emit("calling");
    this.rtc.connect();
  }

  hangup() {
    this.rtc.disconnect();
  }

  stop() {
    this.signaling.stop();
  }

  mediaElement() {
    return this.rtc.getMediaElement();
  }

  // ------------------------------------------------------------------------------
  // Private
  // ------------------------------------------------------------------------------
  _handleJoin(sessionId) {
    this.log.debug("joined session", sessionId);

    const stunServers = [
      {
        urls: ["stun:stun1.l.google.com:19302"]
      }
    ];

    const turnServers = [];

    this.rtc = new IngestCore(null, this.log, this.signaling, {
      sendAudio: true,
      sendVideo: true,
      receiveAudio: false,
      receiveVideo: false,
      videoBitrate: this.bitrate,
      serverConfig: {
        iceServers: Array.prototype.concat(stunServers, turnServers)
      }
    });

    this.rtc.on("userMediaReady", () => this.emit("started"));
    this.rtc.on("userMediaFailed", (error) => this.emit("startFailed", error));
    this.rtc.on("connected", () => this.emit("called"));
    this.rtc.on("connectFailed", (error) => this.emit("callFailed", error));
    this.rtc.on("disconnected", () => this.emit("hangup"));

    this.rtc.requestUserMedia();

    this.emit("ready", { "sessionId": sessionId });
  }
}
