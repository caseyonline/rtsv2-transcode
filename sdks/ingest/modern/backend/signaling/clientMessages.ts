import { StreamIngestProtocol } from "../../../../shared/llnw-types"

import { DataObjectSendMessage
         , DataObjectUpdateMessage } from "../../../../shared/data-object-client-messages";

import { SDPOfferMessage
         , ICECandidateMessage
         , ICEDoneMessage } from "../../../../shared/rtc-client-messages";

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
