/** Provides the API surface of an ingest object. */
export interface IIngest {

  /** Authenticate with the server
   */
  authenticate(username: string, password: string);

  /** Start an ingest
   */
  startIngest(stream: any);

  /** Stop an ingest
   */
  stopIngest();

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

  /** Attaches to the ingest-stats event.  This event is emitted each
   * second with statistics from the underlying peer connection
   */
  on(event: "ingest-audio-stats", handler: (e: any) => void);
  on(event: "ingest-video-stats", handler: (e: any) => void);
}

/** The data provided by a connected event */
export interface IConnectedEventData {
  traceId: string
}

/** The data provided by an authenticated event */
export interface IAuthenticatedEventData {
}
