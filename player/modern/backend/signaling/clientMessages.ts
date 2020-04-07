import { IQualityConstraintConfiguration
         , MessageDestination
         , DataObjectUpdateOperation } from "./types";

export type Message
  = PingMessage
  | SDPOfferMessage
  | ICECandidateMessage
  | ICEDoneMessage
  | SetQualityConstraintConfigurationMessage
  | DataObjectSendMessage
  | DataObjectUpdateMessage

/** A ping message, these should be sent regularly by clients to ensure that the WebSocket connection is left open
 *  if the underlying WebSocket implementation doesn't not automatically generate ping frames (web browsers do not).
 */
export interface PingMessage {
  readonly type: "ping";
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

/** Changes the function of the ABR implementation on the server. */
export interface SetQualityConstraintConfigurationMessage {
  readonly type: "set-quality-constraint-configuration";

  /** The configuration to apply. */
  readonly configuration: IQualityConstraintConfiguration;
}

/** Send a message through the DataObject subsystem. */
export interface DataObjectSendMessage {
  readonly type: "dataobject.send-message";

  /** The message destination **/
  readonly destination: MessageDestination;

  /** The message **/
  readonly msg: string;
}

export interface DataObjectUpdateMessage {
  readonly type: "dataobject.update";

  readonly senderRef: string;

  readonly operation: DataObjectUpdateOperation;
}
