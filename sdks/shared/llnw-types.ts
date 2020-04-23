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

/** Details used to connect to an edge. */
export interface EdgeDetails {

  /** Gets the socket URL that uniquely identifies this edge. This might
   *  be different than the one provided by the user if the edge
   *  redirected the session elsewhere
   */
  readonly socketURL: string;

  /** A list of STUN/TURN servers to provide to the WebRTC infrastructure. */
  readonly iceServers: ICEServerDetails[];

  // /** A URL which provides the stream over HLS if WebRTC isn't available */
  // readonly hlsURL: string;
}
