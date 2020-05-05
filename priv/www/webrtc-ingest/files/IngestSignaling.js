'use strict';

import EventEmitter from "./EventEmitter.js";

const PING_INTERVAL_MS = 5000;
const SCHEDULE_PING_YES = true;
const SCHEDULE_PING_NO = false;

export default class SignalingChannel extends EventEmitter {
  constructor(signalPath) {
    super()

    this.signalPath = signalPath;
    this.pingTimerId = null;
  }

  start(username, password) {
    this.username = username;
    this.password = password;

    var signalSocket = this.signalSocket = new WebSocket(this.signalPath);
    signalSocket.onopen = (event) => this.handleSocketOpen(event);
    signalSocket.onmessage = (event) => this.handleSocketMessage(event);
    signalSocket.onclose = (event) => this.handleSocketClose(event);
  }

  stop() {
    this.signalSocket.close();
  }

  sendRtc(message) {
    this.sendMessage("rtc", {data: message}, SCHEDULE_PING_YES);
  }

  handleSocketOpen(event) {
    var toSend = { "username": this.username, "password": this.password };
    this.sendMessage("join", toSend, SCHEDULE_PING_NO);
    this.emit("ready");
  }

  handleSocketClose(event) {
    this.emit("stopped");
    this.signalSocket = null;
  }

  schedulePing() {
    this.cancelScheduledPing();
    this.pingTimerId = setTimeout(() => this.sendPing(), PING_INTERVAL_MS);
  }

  cancelScheduledPing() {
    if ( this.pingTimerId === null ) {
      return;
    }

    clearTimeout(this.pingTimerId);
    this.pingTimerId = null;
  }

  sendPing() {
    this.sendMessage("ping", {}, SCHEDULE_PING_YES);
  }

  sendMessage(type, data, schedulePing) {
    if (this.signalSocket == null) return;
    var toSend = data;
    toSend["type"] = type;
    this.signalSocket.send(JSON.stringify(toSend));

    if (schedulePing) {
        this.schedulePing();
    }
  }

  handleSocketMessage(message) {
    var response = JSON.parse(event.data);

    switch (response.type) {
    case "rtc":
      this.emit("rtc", response.payload);
      break;
    case "join":
      this.emit("joined", response.session_id);
      this.schedulePing();
      break;
    case "pong":
      break;
    default:
      this.emit("unknown", message);
    }
  }
}
