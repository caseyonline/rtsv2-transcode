/** Details used to connect to an edge. */
export interface EdgeDetails {

  /** Gets the socket URL that uniquely identifies this edge. This might
   *  be different than the one provided by the user if the edge
   *  redirected the session elsewhere
   */
  readonly socketURL: string;

  /** A list of STUN/TURN servers to provide to the WebRTC infrastructure. */
  readonly iceServers: ICEServerDetails[];

  // /** A URL which provides the stream over HLS if WebRTC isn't available */
  // readonly hlsURL: string;
}

/** Details of a STUN/ICE server to be used with WebRTC. */
export interface ICEServerDetails {
  readonly url: string;
  readonly username?: string;
  readonly credential?: string;
}

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

export interface DataObject {
  readonly version: number;
  readonly map: Record<string, any>
}

export type DataObjectUpdateOperation
  = DataObjectInc
  | DataObjectDec
  | DataObjectCAS
  | DataObjectUpdate
  | DataObjectInsert
  | DataObjectRemove;

interface DataObjectInc {
  tag: "inc";
  key: string;
  increment: number;
  initialValue?: number;
}

interface DataObjectDec {
  tag: "dec";
  key: string;
  decrement: number;
  initialValue?: number;
}

interface DataObjectCAS {
  tag: "cas";
  key: string;
  compare: any;
  swap: any;
  createIfMissing: boolean;
}

interface DataObjectUpdate {
  tag: "update";
  key: string;
  value: any;
  createIfMissing: boolean;
}

interface DataObjectInsert {
  tag: "insert";
  key: string;
  value: any;
  failIfPresent: boolean;
}

interface DataObjectRemove {
  tag: "remove";
  key: string;
  failIfMissing: boolean;
}

export type DataObjectUpdateResponse
  = "invalidKey"
  | "invalidOperation"
  | "invalidRequest"
  | "ok"

interface PublisherMessageDestination {
  tag: "publisher";
}

interface BroadcastMessageDestination {
  tag: "broadcast";
}

interface PrivateMessageDestination {
  tag: "private";
  to: string[];
}

export type MessageDestination = PublisherMessageDestination | BroadcastMessageDestination | PrivateMessageDestination
