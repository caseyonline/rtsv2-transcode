import IIngestConfiguration from "../frontend/IIngestConfiguration";

export function withDefaults(configuration: IIngestConfiguration): IIngestConfiguration {
  return {
    account: defaultValue(configuration.account, null),
    streamName: defaultValue(configuration.streamName, null),
    videoElementId: defaultValue(configuration.videoElementId, "llnw-rts-ingest"),
    overrides: defaultOverrides(defaultValue(configuration.overrides, {})),
  };
}

function defaultValue(value, defaultValue) {
  return (value === undefined || value === null) ? defaultValue : value;
}

function defaultOverrides(overrides) {
  return {
    socketAuthority: defaultValue(overrides.socketAuthority, null),
    socketSecure: defaultValue(overrides.socketSecure, null),
    socketPath: defaultValue(overrides.socketPath, null),
  };
}
