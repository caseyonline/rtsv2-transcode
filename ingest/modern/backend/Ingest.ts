import { IIngest, IConnectedEventData } from "../frontend/IIngest";
import EventEmitter from "./util/EventEmitter.ts";

import { ISession } from "./ISession";
import Session from "./Session";

enum IngestState {
  Opening = 1000,
  Connected = 2000,
  Authenticated = 3000
}

export default class Ingest extends EventEmitter implements IIngest {
  private state: IngestState = IngestState.Opening;
  private videoElement: HTMLVideoElement;
  private session: ISession;
  private localStream?: any = null;

  constructor(account: string, streamName: string, socketURL: string, videoElement: HTMLVideoElement) {
    super();
    this.videoElement = videoElement;
    this.session = new Session(socketURL);

    this.session.on("connected", eventData => {
      this.state = IngestState.Connected;
      this.emit("connected", eventData);
    });

    this.session.on("authenticated", eventData => {
      this.state = IngestState.Authenticated;
      this.emit("authenticated", eventData);
    });

    this.session.on("ingest-active", () => {
      this.videoElement.srcObject = this.localStream;
      this.videoElement.play();
      this.emit("ingest-active", {});
    });

    this.session.on("ingest-stopped", () => {
      this.videoElement.srcObject = null;
      this.videoElement.currentTime = 0;
      this.emit("ingest-stopped", {});
    });

    this.session.on("ingest-audio-stats", (stats) => {
      this.emit("ingest-audio-stats", stats);
    });

    this.session.on("ingest-video-stats", (stats) => {
      this.emit("ingest-video-stats", stats);
    });
  }

  authenticate(username: string, password: string) {
    if (this.state != IngestState.Connected) {
      console.warn(`Attempt to authenticate whilst in invalid state ${this.state}`);
      return;
    }
    this.session.authenticate(username, password);
  }

  startIngest(stream: any) {
    if (this.state != IngestState.Authenticated) {
      console.warn(`Attempt to start ingest when not authenticated`);
      return;
    }
    this.localStream = stream;
    this.session.startIngest(stream);
  }

  stopIngest() {
    if (this.state != IngestState.Authenticated) {
      console.warn(`Attempt to stop ingest when not authenticated`);
      return;
    }
    this.localStream.getTracks().forEach((track) => track.stop());
    this.session.stopIngest();
  }
}