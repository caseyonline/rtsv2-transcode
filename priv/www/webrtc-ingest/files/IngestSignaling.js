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
  }

  sendRtc(message) {
    this.sendMessage("rtc", message, SCHEDULE_PING_YES);
  }

  handleSocketOpen(event) {
    var toSend = { "username": this.username, "password": this.password };
    this.sendMessage("join", JSON.stringify(toSend), SCHEDULE_PING_NO);
    this.emit("ready");
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
    this.sendMessage("ping", undefined, SCHEDULE_PING_YES);
  }

  sendMessage(type, data, schedulePing) {
    var toSend = { "type": type, "data": data };
    this.signalSocket.send(JSON.stringify(toSend));

    if (schedulePing) {
        this.schedulePing();
    }
  }

  handleSocketMessage(message) {
    var response = JSON.parse(event.data);

    switch (response.type) {
    case "rtc":
      this.emit("rtc", response.data);
      break;
    case "join":
      this.emit("joined", response.data);
      this.schedulePing();
      break;
    case "pong":
      break;
    default:
      this.emit("unknown", message);
    }
  }
}
