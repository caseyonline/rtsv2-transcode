export default interface ISession {

  // /** Makes a request to the edge to get RTMP connection details
  //  */
  // requestRTMPDetails();

  /** Requests a specific set of constraints on the qualities the client
   *  wishes to received.
   */
  setQualityConstraints(constraints: IQualityConstraintConfiguration);

  /** Attaches to the hello event. This event is emitted by the server
   *  upon successful connection, and provides information necessary
   *  for WebRTC connections
   */
  on(event: "hello", handler: (e: IHelloEventData) => void);

  /** Attaches to the bye event. This event is emitted by the server
   *  to disconnect the client. On receipt the client should disconnect
   *  and reconnect to a different edge.
   */
  on(event: "bye", handler: (e: IByeEventData) => void);

  /** Attaches to the RTMP event. This event is emitted by the server
   *  upon receipt of an RTMP details request to furnish the client
   *  with RTMP endpoint information
   */
  on(event: "rtmp", handler: (e: IRTMPEventData) => void);

  /** Attaches to the candidate event. This event is emitted by the server
   *  when it generates ICE candidates for itself.
   */
  on(event: "candidate", handler: (e: IServerCandidateEventData) => void);

  /** Attaches to the quality change event. This event is emitted by the server
   *  when it changes the quality of the stream being sent to the client.
   */
  on(event: "qualitychange", handler: (e: IQualityChangeEventData) => void);
}

/** The data provided by an hello event. */
export interface IHelloEventData {

  /** Details of the edge to which the client has connected */
  readonly thisEdge: IEdgeDetails;

  /** Details of other edges that could be connected to instead. */
  readonly otherEdges: IEdgeDetails[];
}

/** The data provided by a bye event. */
export interface IByeEventData {

  /** Details of other edges that could be connected to instead. */
  readonly otherEdges: IEdgeDetails[];
}

/** The data provided by an RTMP details event. */
export interface IRTMPEventData {}

/** The data provided by a server ICE candidate details event. */
export interface IServerCandidateEventData {}

/** The data provided by a quality change event. */
export interface IQualityChangeEventData {}

/** Details used to connect to an edge. */
export interface IEdgeDetails {

  /** Gets the socket URL that uniquely identifies this edge. This might
   *  be different than the one provided by the user if the edge
   *  redirected the session elsewhere
   */
  readonly socketURL: string;

  /** A list of STUN servers to provide to the WebRTC infrastructure. */
  readonly stunServers: IStunServerDetails[];

  /** A list of TURN servers to provide to the WebRTC infrastructure. */
  readonly turnServers: ITurnServerDetails[];

  // /** A URL which provides the stream over HLS if WebRTC isn't available */
  // readonly hlsURL: string;
}

/** Details of a STUN server to be used with WebRTC. */
export interface IStunServerDetails {}

/** Details of a TURN server to be used with WebRTC. */
export interface ITurnServerDetails {}

/** Quality constraints to apply to a session. */
export interface IQualityConstraintConfiguration {}

