import { StreamIngestProtocol } from "../../../shared/llnw-types.ts"
import { IIngest } from "../frontend/IIngest";
import { IConnectedEventData } from "../backend/ISession";
import EventEmitter from "../../../shared/util/EventEmitter.ts";

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

    this.session.on("reset", () => {
      this.videoElement.srcObject = null;
      this.videoElement.currentTime = 0;
      if (this.localStream) {
        this.localStream.getTracks().forEach((track) => track.stop());
      }
      this.emit("reset", {});
    });

    this.session.on("ingest-audio-stats", (stats) => {
      this.emit("ingest-audio-stats", stats);
    });

    this.session.on("ingest-video-stats", (stats) => {
      this.emit("ingest-video-stats", stats);
    });

    this.session.on("data-object-message", (message) => {
      this.emit("data-object-message", message);
    });

    this.session.on("data-object", (message) => {
      this.emit("data-object", message);
    });
  }

  authenticate(username: string, password: string, protocol: StreamIngestProtocol) {
    if (this.state != IngestState.Connected) {
      console.warn(`Attempt to authenticate whilst in invalid state ${this.state}`);
      return;
    }
    this.session.authenticate(username, password, protocol);
  }

  startIngest(stream: any, bitrate: number) {
    if (this.state != IngestState.Authenticated) {
      console.warn(`Attempt to start ingest when not authenticated`);
      return;
    }
    this.localStream = stream;
    this.session.startIngest(stream, bitrate);
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
