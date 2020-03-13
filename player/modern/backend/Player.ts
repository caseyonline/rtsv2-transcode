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
}
