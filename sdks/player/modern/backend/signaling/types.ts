/** The behavior of the adaptive bitrate feature in WebRTC */
export enum QualityConstraintBehavior {

  /** adaptive bitrate is turned off, the specified variant will always be sent. */
  ForceQuality = "force-quality",

  /** the specified variant will be use as the *maximum* bitrate, the server can still send lower bitrates as appropriate. */
  MaxQuality = "max-quality"
}

/** Quality constraints to apply to a session. */
export interface IQualityConstraintConfiguration {

  /** The behavior of the adaptive bitrate feature in WebRTC */
  readonly behavior: QualityConstraintBehavior;

  /** The variant to use in conjunction with the behavior property to control which variant(s) the player will received */
  readonly variant : string;
}

const enum RTSStatusCode {
  MessageNotImplemented = 4000,
  MessageBad = 4001,
  InvalidSDP = 4002,
  StreamNotFound = 4003,
  StreamNotReadyRetryLater = 4004,
  AuthenticationFailed = 4005
}