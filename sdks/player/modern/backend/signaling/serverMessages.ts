import { EdgeDetails } from "../../../../shared/llnw-types"

import { SDPOfferResponseMessage
         , ICECandidateMessage } from "../../../../shared/rtc-server-messages"

import { DataObjectReceiveMessage
         , DataObjectSendMessageFailure
         , DataObjectUpdateResponseMessage
         , DataObjectBroadcastMessage
       } from "../../../../shared/data-object-server-messages";

export type Message
  = InitMessage
  | ByeMessage
  | PongMessage
  | QualityChangeMessage
  | SDPOfferResponseMessage
  | ICECandidateMessage
  | OnFIMessage
  | ActiveProfilesMessage
  | DataObjectReceiveMessage
  | DataObjectSendMessageFailure
  | DataObjectUpdateResponseMessage
  | DataObjectBroadcastMessage
  | TimeZeroMessage

/** The data provided by an init event. */
export interface InitMessage {

  readonly type: "init";

  /** Details of the edge to which the client has connected */
  readonly thisEdge: EdgeDetails;

  /** Indicates whether the stream is an audio-only stream */
  readonly audioOnly: boolean;

  /** The variant of the stream that is currently being sent/will be sent to the client
   *  over WebRTC
   */
  readonly activeVariant: string;

  /** The variants of the stream that are available in order from highest-to-lowest quality. */
  readonly variants: string[];

  /** A session-unique identifier that can be used for correlating traces */
  readonly traceId: string;

  /** Media Vault validation cookie */
  readonly validationCookie: string;
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
  readonly validationCookie?: string;
}

/** The data provided by a quality change event - these occur when the server makes abr decisions. */
export interface QualityChangeMessage {
  readonly type: "quality-change";

  /** The name of the variant currently being sent to the client over WebRTC. */
  readonly activeVariant: string;
}

/**  An on-fi message originating from the source ingest. */
export interface OnFIMessage {
  readonly type: "on-fi";

  /** The source encoder timestamp. */
  readonly payload: any;

  /** The corresponding video timestamp. */
  readonly pts: number;
}

/** An active-profiles message originating from the ingest aggregator. */
export interface ActiveProfilesMessage {
  readonly type: "active-profiles";

  /** The active profiles. */
  readonly activeProfiles: Array<string>;
}

/** The RTP timestamp corresponding to the first frame sent. */
export interface TimeZeroMessage {
  readonly type: "time-zero";

  /** The RTP timestamp corresponding to the first frame sent. */
  readonly rtpTimestamp: number;
}
