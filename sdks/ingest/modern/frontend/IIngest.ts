import { StreamIngestProtocol } from "../../../shared/llnw-types";

import { DataObject } from "../../../shared/data-object-types";

import { DataObjectReceiveMessage } from "../../../shared/data-object-server-messages";

import { IConnectedEventData, IAuthenticatedEventData } from "../backend/ISession";

/** Provides the API surface of an ingest object. */
export interface IIngest {

  /** Authenticate with the server
   */
  authenticate(username: string, password: string, protocol: StreamIngestProtocol);

  /** Start an ingest
   */
  startIngest(stream: any, bitrate: number);

  /** Stop an ingest
   */
  stopIngest();

  /** Data Object functions
   */
  sendPrivateMessage(to: string[], msg: string);
  sendBroadcastMessage(msg: string);
  dataObjectInc(keys: string[], increment: number, senderRef: string, createIfKeyMissing: boolean);
  dataObjectDec(keys: string[], decrement: number, senderRef: string, createIfKeyMissing: boolean);
  dataObjectCAS(keys: string[], compare: any, swap: any, createIfKeyMissing: boolean, senderRef: string);
  dataObjectAdd(keys: string[], value: any, failIfKeyPresent: boolean, senderRef: string);
  dataObjectUpdate(keys: string[], value: any, createIfKeyMissing: boolean, senderRef: string);
  dataObjectDelete(keys: string[], failIfKeyMissing: boolean, senderRef: string);
  dataObjectListInsert(keys: string[], value: any, createIfKeyMissing: boolean, failIfValuePresent: boolean, senderRef: string);
  dataObjectListRemove(keys: string[], value: any, failIfKeyMissing: boolean, failIfValueMissing: boolean, senderRef: string);

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

  /** Attaches to the reset event.  This event is emitted upon ingest
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

  /** Attaches to the data-object event.  This event is emitted upon
   * receipt of the latest data object
   */
  on(event: "data-object", handler: (message: DataObject) => void);
}
