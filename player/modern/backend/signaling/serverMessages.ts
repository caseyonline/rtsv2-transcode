import { EdgeDetails
         , DataObjectUpdateResponse
         , DataObject } from "./types";

export type Message
  = InitMessage
  | ByeMessage
  | PongMessage
  | QualityChangeMessage
  | SDPOfferResponseMessage
  | ICECandidateMessage
  | OnFIMessage
  | DataObjectReceiveMessage
  | DataObjectUpdateResponseMessage
  | DataObjectBroadcastMessage

/** The data provided by an init event. */
export interface InitMessage {

  readonly type: "init";

  /** Details of the edge to which the client has connected */
  readonly thisEdge: EdgeDetails;

  /** The variant of the stream that is currently being sent/will be sent to the client
   *  over WebRTC
   */
  readonly activeVariant: string;

  /** The variants of the stream that are available in order from highest-to-lowest quality. */
  readonly variants: string[];

  /** A session-unique identifier that can be used for correlating traces */
  readonly traceId: string;
}

/** The data provided by a bye event. */
export interface ByeMessage {

  readonly type: "bye";

  /** Details of other edges that could be connected to instead. */
  readonly otherEdges: EdgeDetails[];
}

/** The data provided by a pong event - these occur in response to application-level pings from the client. */
export interface PongMessage {
  readonly type: "pong";
}

/** The data provided by a quality change event - these occur when the server makes abr decisions. */
export interface QualityChangeMessage {
  readonly type: "quality-change";

  /** The name of the variant currently being sent to the client over WebRTC. */
  readonly activeVariant: string;
}

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

/**  An on-fi message originating from the source ingest. */
export interface OnFIMessage {
  readonly type: "on-fi";

  /** The source encoder timestamp. */
  readonly timestamp: number;

  /** The corresponding video timestamp. */
  readonly pts: number;
}

/** Receive a message from the DataObject subsystem. */
export interface DataObjectReceiveMessage {
  readonly type: "dataobject.message";

  /** The message sended **/
  readonly sender: string

  /** The message **/
  readonly msg: string
}

export interface DataObjectUpdateResponseMessage {
  readonly type: "dataobject.update-response";

  readonly senderRef: string;

  readonly response: DataObjectUpdateResponse;
}

export interface DataObjectBroadcastMessage {
  readonly type: "dataobject.broadcast";

  readonly object: DataObject;
}
