import { IngestDetails } from "../../../../shared/llnw-types"

import { SDPOfferResponseMessage
         , ICECandidateMessage } from "../../../../shared/rtc-server-messages"

import { DataObjectReceiveMessage
         , DataObjectUpdateResponseMessage
         , DataObjectBroadcastMessage
       } from "../../../../shared/data-object-server-messages";

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

/**  An on-fi message originating from the source ingest. */
export interface OnFIMessage {
  readonly type: "on-fi";

  /** The source encoder timestamp. */
  readonly timestamp: number;

  /** The corresponding video timestamp. */
  readonly pts: number;
}
