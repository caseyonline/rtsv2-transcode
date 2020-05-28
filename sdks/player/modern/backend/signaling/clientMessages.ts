import { IQualityConstraintConfiguration } from "./types";

import { DataObjectSendMessage
         , DataObjectUpdateMessage } from "../../../../shared/data-object-client-messages";

import { SDPOfferMessage
         , ICECandidateMessage
         , ICEDoneMessage } from "../../../../shared/rtc-client-messages";

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
  readonly validationCookie?: string;
}

/** Changes the function of the ABR implementation on the server. */
export interface SetQualityConstraintConfigurationMessage {
  readonly type: "set-quality-constraint-configuration";

  /** The configuration to apply. */
  readonly configuration: IQualityConstraintConfiguration;
}
