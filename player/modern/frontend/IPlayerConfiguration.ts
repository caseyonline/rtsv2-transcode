/** Provides a type for the configuration required
 *  by a player to connect to a stream
 */
export default interface IPlayerConfiguration {

  /** Your account name, as provided in your welcome letter */
  readonly account: string;

  /** The stream name, as passed as the name parameter in the
   *  call to create a new slot, or available from the details
   *  page of the Slot in the Limelight Control Portal.
   */
  readonly streamName: string;

  /** The id of the video element to use for playback. Defaults
   *  to llnw-rts-subscriber
   */
  readonly videoElementId?: string;

  // /** The order in which fallback will occur if certain playback
  //  *  options are not available.
  //  */
  // readonly fallbackOrder: FallbackOption[]
  //

  /** Whether to allow the SDK to automatically adjust
   *  the video orientation if someone is streaming
   *  from a phone, which may broadcast in both landscape
   *  and portrait orientation. Defaults to true
   */
  readonly autoLayoutOrientation?: boolean;

  // Overrides. Not for end-user usage.
  // TOOD: remove from release builds?
  readonly overrides?: IPlayerOverrides;
}

// Overrides. Not for end-user usage.
// TOOD: remove from release builds?
export interface IPlayerOverrides {
  socketAuthority?: string;
  socketSecure?: boolean;
  socketPath?: string;
}
