import { StreamIngestProtocol } from "../../../shared/llnw-types";

import { DataObject
         , MessageDestination
         , DataObjectUpdateOperation
       } from "../../../shared/data-object-types";

import { DataObjectReceiveMessage
         , DataObjectUpdateResponseMessage
       } from "../../../shared/data-object-server-messages";

export interface ISession {
  /** Authenticate with the server
   */
  authenticate(username: string, password: string, protocol: StreamIngestProtocol);

  /** Start an ingest
   */
  startIngest(stream: any, bitrate: number);

  /** Stop an ingest
   */
  stopIngest();

  /** Data object functions
   */
  sendMessage(destination: MessageDestination, msg: string);
  sendUpdate(operation: DataObjectUpdateOperation, senderRef: string);

  /** Attaches to the connected event.  This event is emitted upon
   * successful websocket connection
   */
  on(event: "connected", handler: (e: IConnectedEventData) => void);

  /** Attaches to the authenticated event.  This event is emitted upon
   * successful authentication
   */
  on(event: "authenticated", handler: (e: IAuthenticatedEventData) => void);

  /** Attaches to the ingest-active event.  This event is emitted upon
   * successful ingest initiation
   */
  on(event: "ingest-active", handler: () => void);

  /** Attaches to the ingest-stopped event.  This event is emitted upon
   * ingest stop
   */
  on(event: "ingest-stopped", handler: () => void);

  /** Attaches to the reset event.  This event is emitted upon session
   * reset
   */
  on(event: "reset", handler: () => void);

  /** Attaches to the ingest-stats event.  This event is emitted each
   * second with statistics from the underlying peer connection
   */
  on(event: "ingest-audio-stats", handler: (e: any) => void);
  on(event: "ingest-video-stats", handler: (e: any) => void);

  /** Attaches to the data-object-message event.  This event is emitted upon
   * receipt of a message sent to the publisher
   */
  on(event: "data-object-message", handler: (message: DataObjectReceiveMessage) => void);

  /** Attaches to the data-object-update-response event.  This event is emitted upon
   * receipt of an update response
   */
  on(event: "data-object-update-response", handler: (message: DataObjectUpdateResponseMessage) => void);

  /** Attaches to the data-object event.  This event is emitted upon
   * receipt of the latest data object
   */
  on(event: "data-object", handler: (message: DataObject) => void);
}

/** The data provided by a connected event */
export interface IConnectedEventData {
  traceId: string
}

/** The data provided by an authenticated event */
export interface IAuthenticatedEventData {
}
