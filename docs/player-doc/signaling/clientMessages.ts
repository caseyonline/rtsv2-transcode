import { IQualityConstraintConfiguration } from "./types";

export interface IClientEventData {
  readonly type: string;
}

/** A ping message, these should be sent regularly by clients to ensure that the WebSocket connection is left open
 *  if the underlying WebSocket implementation doesn't not automatically generate ping frames (web browsers do not).
 */
export interface IPingClientEventData extends IClientEventData {
  readonly type: "ping";
}

/** Sends an SDP offer from the WebRTC implementation on the client to the server. */
export interface ISDPOfferResponseClientEventData extends IServerEventData {
  readonly type: "sdp.offer";

  /** The SDP which describes the offer from the client to the server. */
  readonly offer: string;
}

/** Sends an ICE candidate from the WebRTC implementation on the client to the server. */
export interface IICECandidateResponseClientEventData extends IServerEventData {
  readonly type: "ice.candidate";

  /** The candidate line. */
  readonly candidate: string;

  /** The media line index to which the candidate applies. */
  readonly index: number;
}

/** Notifies the server that ICE candidate gathering is done. */
export interface IICEDoneResponseClientEventData extends IServerEventData {
  readonly type: "ice.done";
}

/** Changes the function of the ABR implementation on the server. */
export interface ISetQualityConstraintConfigurationClientEventData extends IClientEventData {
  readonly type: "set-quality-constraint-configuration";

  /** The configuration to apply. */
  readonly configuration: IQualityConstraintConfiguration;
}
