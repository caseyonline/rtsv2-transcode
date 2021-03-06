// https://tools.ietf.org/html/rfc6455#page-45
export const enum WebSocketProtocolStatusCode {
  NormalClosure = 1000,
  GoingAway = 1001,
  ProtocolError = 1002,
  DataNotAcceptable = 1003,
  Reserved1 = 1004,
  Reserved2 = 1005,

  // This is a reserved code that a server MUST NOT send.
  // Browsers use this to flag that a connection closed without
  // a close frame.
  Client_ConnectionClosedAbnormally = 1006,

  DataInconsistentWithType = 1007,
  PolicyViolation = 1008,
  TooBig = 1009,

  // Generated by a client to indicate the server
  // didn't respond with a required extension
  Client_ExpectedExtension = 1010,

  UnexpectedCondition = 1011,

  // This is a reserved code that a server MUST NOT send.
  // Browsers use this to flag that a connection is closed
  // due to a TLS handshake failure.
  Client_TLSFailure = 1015
}

export const enum WebSocketProtocolStatusRange {

  // Codes that aren't in any range in the specification.
  Invalid,

  // 0-999 are unused
  NotUsed,

  // 1000-2999 are reserved for the definition of the
  // Web Sockets Protocol
  Protocol,

  // 3000-3999 are for use by libraries, frameworks,
  // and applications, and are registered with IANA
  Registered,

  // 4000-4999 are for private use by applications and
  // are not registered.
  Applications,
}

export function classifyStatusCode(code: number): WebSocketProtocolStatusRange {
  if ( code >= 0 && code <= 999 ) {
    return WebSocketProtocolStatusRange.NotUsed;
  }
  else if ( code >= 1000 && code <= 2999 ) {
    return WebSocketProtocolStatusRange.Protocol;
  }
  else if ( code >= 3000 && code <= 3999 ) {
    return WebSocketProtocolStatusRange.Registered;
  }
  else if ( code >= 4000 && code <= 4999 ) {
    return WebSocketProtocolStatusRange.Applications;
  }
  else {
    return WebSocketProtocolStatusRange.Invalid;
  }
}
