import { DataObjectUpdateResponse
         , DataObject } from "./data-object-types";

/** Receive a message from the DataObject subsystem. */
export interface DataObjectReceiveMessage {
  readonly type: "dataobject.message";

  /** The message sended **/
  readonly sender: string

  /** The message **/
  readonly msg: string
}

/** Receive a response to a previous update. */
export interface DataObjectUpdateResponseMessage {
  readonly type: "dataobject.update-response";

  /** An opaque reference as sent on the request. */
  readonly senderRef: string;

  /** The response code. */
  readonly response: DataObjectUpdateResponse;
}

/** Receive the latest DataObject. */
export interface DataObjectBroadcastMessage {
  readonly type: "dataobject.broadcast";

  /** The DataObject. */
  readonly object: DataObject;
}
