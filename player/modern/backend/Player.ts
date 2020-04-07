import IPlayer from "../frontend/IPlayer";

import Session from "./Session";
import { QualityConstraintBehavior } from "./signaling/types";

export default class Player implements IPlayer {
  private account: string;
  private stream: string;
  private videoElement: HTMLVideoElement;
  private session: Session;

  constructor(account: string, stream: string, socketURL: string, videoElement: HTMLVideoElement) {
    this.account = account;
    this.stream = stream;
    this.videoElement = videoElement;
    this.session = new Session(socketURL);
    this.session.on("stream", stream => {
      console.debug("Received stream from session manager.");
      this.videoElement.srcObject = stream;
    });
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

  dataObjectInc(key: string, increment: number, senderRef: string, initialValue?: number) {
    this.session.sendUpdate({ tag: "inc"
                              , key: key
                              , increment: increment
                              , initialValue: initialValue
                            }, senderRef);
  }

  dataObjectDec(key: string, decrement: number, senderRef: string, initialValue?: number) {
    this.session.sendUpdate({ tag: "dec"
                              , key: key
                              , decrement: decrement
                              , initialValue: initialValue
                            }, senderRef);
  }

  dataObjectCAS(key: string, compare: any, swap: any, createIfMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "cas"
                              , key: key
                              , compare: compare
                              , swap: swap
                              , createIfMissing: createIfMissing
                            }, senderRef);
  }

  dataObjectUpdate(key: string, value: any, createIfMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "update"
                              , key: key
                              , value: value
                              , createIfMissing: createIfMissing
                            }, senderRef);
  }

  dataObjectInsert(key: string, value: any, failIfPresent: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "insert"
                              , key: key
                              , value: value
                              , failIfPresent: failIfPresent
                            }, senderRef);
  }

  dataObjectRemove(key: string, failIfMissing: boolean, senderRef: string) {
    this.session.sendUpdate({ tag: "remove"
                              , key: key
                              , failIfMissing: failIfMissing
                            }, senderRef);
  }
}
