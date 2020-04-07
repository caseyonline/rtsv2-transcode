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
