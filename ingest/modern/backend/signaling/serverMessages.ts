import { IngestDetails
         , DataObjectUpdateResponse
         , DataObject } from "./types";

export type Message
  = InitMessage
  | AuthenticatedMessage
  | IngestStartedMessage
  | IngestStoppedMessage
  | ByeMessage
  | PongMessage
  | SDPOfferResponseMessage
  | ICECandidateMessage
  | OnFIMessage
  | DataObjectReceiveMessage
  | DataObjectUpdateResponseMessage
  | DataObjectBroadcastMessage

/** The data provided by an init event. */
export interface InitMessage {

  readonly type: "init";

  /** A session-unique identifier that can be used for correlating traces */
  readonly traceId: string;
}

/** The data provided by an authenticated event. */
export interface AuthenticatedMessage {

  readonly type: "authenticated";

  /** Details of the ingest to which the client has connected */
  readonly thisIngest: IngestDetails;
}

/** The data provided by an ingest-started event. */
export interface IngestStartedMessage {

  readonly type: "ingest-started";
}

/** The data provided by an ingest-stop event. */
export interface IngestStoppedMessage {

  readonly type: "ingest-stopped";
}

/** The data provided by a bye event. */
export interface ByeMessage {

  readonly type: "bye";
}

/** The data provided by a pong event - these occur in response to application-level pings from the client. */
export interface PongMessage {
  readonly type: "pong";
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

/** Receive a response to a previous update. */
export interface DataObjectUpdateResponseMessage {
  readonly type: "dataobject.update-response";

  /** An opaque reference as sent on the request. */
  readonly senderRef: string;

  /** The response code. */
  readonly response: DataObjectUpdateResponse;
}

/** Receive the latest DataObject. */
export interface DataObjectBroadcastMessage {
  readonly type: "dataobject.broadcast";

  /** The DataObject. */
  readonly object: DataObject;
}
