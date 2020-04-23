import { ICEServerDetails } from "./rtc-types.ts"

export const enum StreamIngestProtocol {
  webrtc = "webrtc",
  rtmp = "rtmp"
}

/** Details used to connect to an ingest. */
export interface IngestDetails {

  /** A list of STUN/TURN servers to provide to the WebRTC infrastructure. */
  readonly iceServers: ICEServerDetails[];
}
