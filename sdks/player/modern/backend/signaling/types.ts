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

/** Top level data object type. */
export interface DataObject {
  readonly version: number;
  readonly map: Record<string, any>
}

/** Data object update operations. */
export type DataObjectUpdateOperation
  = DataObjectInc
  | DataObjectDec
  | DataObjectCAS
  | DataObjectAdd
  | DataObjectUpdate
  | DataObjectDelete
  | DataObjectListInsert
  | DataObjectListRemove
;

/** Increment - increment a counter by 'increment';  if counter does
 * not exist, then it is created with initial value 'increment', if
 * createIfKeyMissing is true.
 */
interface DataObjectInc {
  tag: "inc";
  keys: string[];
  increment: number;
  createIfKeyMissing: boolean;
}

/** Decrement - decrement a counter by 'decrement';  if counter does
 * not exist, then it is created with initial value '-decrement', if
 * createIfKeyMissing is true.
 */
interface DataObjectDec {
  tag: "dec";
  keys: string[];
  decrement: number;
  createIfKeyMissing: boolean;
}

/** CompareAndSwap - for the key, compare the current value to 'compare'; if
 * it matches, then replace it with the value 'swap'.  If key does
 * not exist and createIfKeyMissing is true, then key is created with
 * value 'swap'.
 */
interface DataObjectCAS {
  tag: "cas";
  keys: string[];
  compare: any;
  swap: any;
  createIfKeyMissing: boolean;
}

/** Add - add a new key, setting it to 'value'.  If the key already exists, and if
 * failIfKeyPresent is true, then update fails.
 */
interface DataObjectAdd {
  tag: "add";
  keys: string[];
  value: any;
  failIfKeyPresent: boolean;
}

/** Update - update a key, setting it to 'value'.  If the key is missing
 * and createIfKeyMissing is true, then the key is created.
 */
interface DataObjectUpdate {
  tag: "update";
  keys: string[];
  value: any;
  createIfKeyMissing: boolean;
}

/** Delete - delete a key.  If the key does not exist and failIfKeyMissing is
 * true, then the request fails.
 */
interface DataObjectDelete {
  tag: "delete";
  keys: string[];
  failIfKeyMissing: boolean;
}

/** ListInsert - insert a value into the list stored at key. If the
 * key does not exist and createIfKeyMissing is true, then the key is created.
 * If the value already exists in the list, and failIfValuePresent is true,
 * then the request fails.
 */
interface DataObjectListInsert {
  tag: "list.insert";
  keys: string[];
  value: any;
  createIfKeyMissing: boolean;
  failIfValuePresent: boolean;
}

/** ListRemove - removes a value into the list stored at key. If the
 * key does not exist and failedIfKeyMissing is true, then the request fails.
 * If the value is not present in the list, and failIfValueMissing is true,
 * then the request fails.
 */
interface DataObjectListRemove {
  tag: "list.remove";
  keys: string[];
  value: any;
  failIfKeyMissing: boolean;
  failIfValueMissing: boolean;
}

export type DataObjectUpdateResponse
  = "invalidKey"             /* The key requested does not exist. */
  | "invalidValue"           /* For list operations, the value was either already present (insert) or not present (delete). */
  | "invalidOperation"       /* An invalid operation was requested for the type of the key (e.g., attempting to increment a list). */
  | "compareAndSwapFailed"   /* A compare-and-swap operation fail because the 'compare' value did not match. */
  | "pendingInitialisation"  /* The system is currently initialising.  This is transient, just retry. */
  | "pendingSynchronisation" /* The system is currently synchronising.  This is transient, just retry. */
  | "networkError"           /* An unexpected network condition occurred.  This is transient, just retry. */
  | "unexpected"             /* The system is in an unexpected state.  Retrying may or may not work.  Report this if you see it. */
  | "ok"                     /* The request succeeded. */

/** Send message to the publisher. */
interface PublisherMessageDestination {
  tag: "publisher";
}

/** Send message to all viewers. */
interface BroadcastMessageDestination {
  tag: "broadcast";
}

/** Send message to specific viewers. */
interface PrivateMessageDestination {
  tag: "private";
  to: string[];
}

export type MessageDestination = PublisherMessageDestination | BroadcastMessageDestination | PrivateMessageDestination
