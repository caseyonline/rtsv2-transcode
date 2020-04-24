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
    this.session.on("dataobject.message", (message) => {
      this.emit("dataobject.message", message);
    });
    this.session.on("dataobject.update-response", (response) => {
      this.emit("dataobject.update-response", response);
    });
    this.session.on("dataobject.broadcast", (dataObject) => {
      this.emit("dataobject.broadcast", dataObject);
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

  dataObjectInc(keys: string[], increment: number, senderRef: string, createIfKeyMissing: boolean) {
    this.session.sendUpdate({ tag: "inc"
                              , keys: keys
                              , increment: increment
                              , createIfKeyMissing: createIfKeyMissing
                            }, senderRef);
  }

  dataObjectDec(keys: string[], decrement: number, senderRef: string, createIfKeyMissing: boolean) {
    this.session.sendUpdate({ tag: "dec"
                              , keys: keys
                              , decrement: decrement
                              , createIfKeyMissing: createIfKeyMissing
                            }, senderRef);
  }

  dataObjectCAS(keys: string[], compare: any, swap: any, createIfKeyMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "cas"
                              , keys: keys
                              , compare: compare
                              , swap: swap
                              , createIfKeyMissing: createIfKeyMissing
                            }, senderRef);
  }

  dataObjectAdd(keys: string[], value: any, failIfKeyPresent: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "add"
                              , keys: keys
                              , value: value
                              , failIfKeyPresent: failIfKeyPresent
                            }, senderRef);
  }


  dataObjectUpdate(keys: string[], value: any, createIfKeyMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "update"
                              , keys: keys
                              , value: value
                              , createIfKeyMissing: createIfKeyMissing
                            }, senderRef);
  }

  dataObjectDelete(keys: string[], failIfKeyMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "delete"
                              , keys: keys
                              , failIfKeyMissing: failIfKeyMissing
                            }, senderRef);
  }

  dataObjectListInsert(keys: string[], value: any, createIfKeyMissing: boolean, failIfValuePresent: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "list.insert"
                              , keys: keys
                              , value: value
                              , createIfKeyMissing: createIfKeyMissing
                              , failIfValuePresent: failIfValuePresent
                            }, senderRef);
  }

  dataObjectListRemove(keys: string[], value: any, failIfKeyMissing: boolean, failIfValueMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "list.remove"
                              , keys: keys
                              , value: value
                              , failIfKeyMissing: failIfKeyMissing
                              , failIfValueMissing: failIfValueMissing
                            }, senderRef);
  }
}
