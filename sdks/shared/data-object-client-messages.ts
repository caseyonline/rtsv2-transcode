import { MessageDestination
         , DataObjectUpdateOperation } from "./data-object-types";


/** Send a message through the DataObject subsystem. */
export interface DataObjectSendMessage {
  readonly type: "dataobject.send-message";

  /** The message destination **/
  readonly destination: MessageDestination;

  /** The message **/
  readonly msg: string;
}

/** Perform a data object update. */
export interface DataObjectUpdateMessage {
  readonly type: "dataobject.update";

  /** An opaque reference returned in the response. */
  readonly senderRef: string;

  /** The operation to perform. */
  readonly operation: DataObjectUpdateOperation;
}
