import { IEdgeDetails } from "./types";

export interface IServerEventData {
  readonly type: string;
}

/** The data provided by an init event. */
export interface IInitServerEventData extends IServerEventData {

  readonly type: "init";

  /** Details of the edge to which the client has connected */
  readonly thisEdge: IEdgeDetails;

  /** The variant of the stream that is currently being sent/will be sent to the client
   *  over WebRTC
   */
  readonly activeVariant: string;

  /** The variants of the stream that are available in order from highest-to-lowest quality. */
  readonly variants: string[];
}

/** The data provided by a bye event. */
export interface IByeServerEventData extends IServerEventData {

  readonly type: "bye";

  /** Details of other edges that could be connected to instead. */
  readonly otherEdges: IEdgeDetails[];
}

/** The data provided by a quality change event - these occur when the server makes abr decisions. */
export interface IQualityChangeEventData extends IServerEventData {
  readonly type: "quality-change";

  /** The name of the variant currently being sent to the client over WebRTC. */
  readonly activeVariant: string;
}

/** The data provided by a pong event - these occur in response to application-level pings from the client. */
export interface IPingEventData extends IServerEventData {
  readonly type: "ping";
}

/** The response to an SDP offer sent from the client. */
export interface ISDPOfferResponseServerEventData extends IServerEventData {
  readonly type: "sdp.offer-response";

  /** The SDP which describes the response from the server to the client's offer. */
  readonly response: string;
}

/**  A candidate from the WebRTC implementation on the server for the client to use in its ICE process. */
export interface IICECandidateResponseClientEventData extends IServerEventData {
  readonly type: "ice.candidate";

  /** The candidate line. */
  readonly candidate: string;

  /** The media line index to which the candidate applies. */
  readonly index: number;
}
