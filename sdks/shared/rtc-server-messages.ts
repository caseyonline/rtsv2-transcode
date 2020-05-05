/** The response to an SDP offer sent from the client. */
export interface SDPOfferResponseMessage {
  readonly type: "sdp.offer-response";

  /** The SDP which describes the response from the server to the client's offer. */
  readonly response: string;
}

/**  A candidate from the WebRTC implementation on the server for the client to use in its ICE process. */
export interface ICECandidateMessage {
  readonly type: "ice.candidate";

  /** The candidate line. */
  readonly candidate: string;

  /** The media line index to which the candidate applies. */
  readonly index: number;
}
