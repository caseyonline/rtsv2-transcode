/** Sends an SDP offer from the WebRTC implementation on the client to the server. */
export interface SDPOfferMessage {
  readonly type: "sdp.offer";

  /** The SDP which describes the offer from the client to the server. */
  readonly offer: string;
}

/** Sends an ICE candidate from the WebRTC implementation on the client to the server. */
export interface ICECandidateMessage {
  readonly type: "ice.candidate";

  /** The candidate line. */
  readonly candidate: string;

  /** The media line index to which the candidate applies. */
  readonly index: number;
}

/** Notifies the server that ICE candidate gathering is done. */
export interface ICEDoneMessage {
  readonly type: "ice.done";
}
