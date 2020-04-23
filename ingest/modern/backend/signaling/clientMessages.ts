import { MessageDestination
         , DataObjectUpdateOperation
         , StreamIngestProtocol } from "./types";

export type Message
  = PingMessage
  | AuthenticateMessage
  | StartIngestMessage
  | StopIngestMessage
  | SDPOfferMessage
  | ICECandidateMessage
  | ICEDoneMessage
  | DataObjectSendMessage
  | DataObjectUpdateMessage

/** A ping message, these should be sent regularly by clients to ensure that the WebSocket connection is left open
 *  if the underlying WebSocket implementation doesn't not automatically generate ping frames (web browsers do not).
 */
export interface PingMessage {
  readonly type: "ping";
}

/** Authenticate with the server. */
export interface AuthenticateMessage {
  readonly type: "authenticate";

  /** The username. */
  readonly username: string;

  /** The password. */
  readonly password: string;

  /** The protocol. */
  readonly protocol: StreamIngestProtocol;
}

/** Request an ingest start. */
export interface StartIngestMessage {
  readonly type: "start-ingest";
}

/** Request an ingest start. */
export interface StopIngestMessage {
  readonly type: "stop-ingest";
}

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

/** Send a message through the DataObject subsystem. */
export interface DataObjectSendMessage {
  readonly type: "dataobject.send-message";

  /** The message destination **/
  readonly destination: MessageDestination;

  /** The message **/
  readonly msg: string;
}

/** Perform a data object update. */
export interface DataObjectUpdateMessage {
  readonly type: "dataobject.update";

  /** An opaque reference returned in the response. */
  readonly senderRef: string;

  /** The operation to perform. */
  readonly operation: DataObjectUpdateOperation;
}
