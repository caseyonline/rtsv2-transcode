import IPlayer from "../frontend/IPlayer";

import Session from "./Session";
import { QualityConstraintBehavior } from "./signaling/types";
import EventEmitter from "../../../shared/util/EventEmitter.ts";

export default class Player extends EventEmitter implements IPlayer {
  private account: string;
  private stream: string;
  private videoElement: HTMLVideoElement;
  private session: Session;
  private localStream?: any = null;

  constructor(account: string, stream: string, socketURL: string, videoElement: HTMLVideoElement) {
    super();
    this.account = account;
    this.stream = stream;
    this.videoElement = videoElement;
    this.session = new Session(socketURL);
    this.session.on("stream", stream => {
      console.debug("Received stream from session manager.");
      this.videoElement.srcObject = stream;
      this.localStream = stream;
    });
    this.session.on("quality-change", (message) => {
      this.emit("quality-change", message);
    });
    this.session.on("on-fi", (message) => {
      this.emit("on-fi", message);
    });
    this.session.on("active-profiles", (profiles) => {
      this.emit("active-profiles", profiles);
    });
    this.session.on("data-object-message", (message) => {
      this.emit("data-object-message", message);
    });
    this.session.on("data-object-message-failure", (message) => {
      this.emit("data-object-message-failure", message);
    });
    this.session.on("data-object-update-response", (response) => {
      this.emit("data-object-update-response", response);
    });
    this.session.on("data-object", (dataObject) => {
      this.emit("data-object", dataObject);
    });
    this.session.on("playback-active", () => {
      this.emit("playback-active", {});
    });
    this.session.on("playback-audio-stats", (stats) => {
      this.emit("playback-audio-stats", stats);
    });
    this.session.on("playback-video-stats", (stats) => {
      this.emit("playback-video-stats", stats);
    });
    this.session.on("time-zero", (message) => {
      this.emit("time-zero", message);
    });
  }

  stop() {
    this.session.stop();
    this.videoElement.srcObject = null;
    this.videoElement.currentTime = 0;
    if (this.localStream) {
      this.localStream.getTracks().forEach((track) => track.stop());
    }
  }

  requestMigrate(socketURL: string) {
    this.session.requestMigrate(socketURL);
  }

  setProfile(profileName: string) {
    this.session.setQualityConstraint({ behavior: QualityConstraintBehavior.ForceQuality, variant: profileName });
  }

  sendPrivateMessage(to: string[], msg: string) {
    this.session.sendMessage({ tag: "private", to: to }, msg );
  }

  sendPublisherMessage(msg: string) {
    this.session.sendMessage({ tag: "publisher" }, msg );
  }

  sendBroadcastMessage(msg: string) {
    this.session.sendMessage({ tag: "broadcast" }, msg );
  }

  dataObjectInc(keys: string[], increment: number, requestResponseCorrelationId: string, createIfKeyMissing: boolean) {
    this.session.sendUpdate({ tag: "inc"
                              , keys: keys
                              , increment: increment
                              , createIfKeyMissing: createIfKeyMissing
                            }, requestResponseCorrelationId);
  }

  dataObjectDec(keys: string[], decrement: number, requestResponseCorrelationId: string, createIfKeyMissing: boolean) {
    this.session.sendUpdate({ tag: "dec"
                              , keys: keys
                              , decrement: decrement
                              , createIfKeyMissing: createIfKeyMissing
                            }, requestResponseCorrelationId);
  }

  dataObjectCAS(keys: string[], compare: any, swap: any, createIfKeyMissing: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "cas"
                              , keys: keys
                              , compare: compare
                              , swap: swap
                              , createIfKeyMissing: createIfKeyMissing
                            }, requestResponseCorrelationId);
  }

  dataObjectAdd(keys: string[], value: any, failIfKeyPresent: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "add"
                              , keys: keys
                              , value: value
                              , failIfKeyPresent: failIfKeyPresent
                            }, requestResponseCorrelationId);
  }


  dataObjectUpdate(keys: string[], value: any, createIfKeyMissing: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "update"
                              , keys: keys
                              , value: value
                              , createIfKeyMissing: createIfKeyMissing
                            }, requestResponseCorrelationId);
  }

  dataObjectDelete(keys: string[], failIfKeyMissing: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "delete"
                              , keys: keys
                              , failIfKeyMissing: failIfKeyMissing
                            }, requestResponseCorrelationId);
  }

  dataObjectListInsert(keys: string[], value: any, createIfKeyMissing: boolean, failIfValuePresent: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "list.insert"
                              , keys: keys
                              , value: value
                              , createIfKeyMissing: createIfKeyMissing
                              , failIfValuePresent: failIfValuePresent
                            }, requestResponseCorrelationId);
  }

  dataObjectListRemove(keys: string[], value: any, failIfKeyMissing: boolean, failIfValueMissing: boolean, requestResponseCorrelationId: string) {
    this.session.sendUpdate({ tag: "list.remove"
                              , keys: keys
                              , value: value
                              , failIfKeyMissing: failIfKeyMissing
                              , failIfValueMissing: failIfValueMissing
                            }, requestResponseCorrelationId);
  }
}
