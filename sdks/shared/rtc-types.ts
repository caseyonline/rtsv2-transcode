/** Details of a STUN/ICE server to be used with WebRTC. */
export interface ICEServerDetails {
  readonly url: string;
  readonly username?: string;
  readonly credential?: string;
}
