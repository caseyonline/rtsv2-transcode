/** Provides a type for the configuration required
 *  by an ingest
 */
export default interface IIngestConfiguration {
  /** Your account name */
  readonly account: string;

  /** The stream name, as passed as the name parameter in the
   *  call to create a new slot, or available from the details
   *  page of the Slot in the Limelight Control Portal.
   */
  readonly streamName: string;

  /** The id of the video element to use for mirror playback. Defaults
   *  to llnw-rts-ingest
   */
  readonly videoElementId?: string;

  // Overrides. Not for end-user usage.
  // TOOD: remove from release builds?
  readonly overrides?: IIngestOverrides;
}

// Overrides. Not for end-user usage.
// TOOD: remove from release builds?
export interface IIngestOverrides {
  socketAuthority?: string;
  socketSecure?: boolean;
  socketPath?: string;
}
