import IPlayerConfiguration from "../frontend/IPlayerConfiguration";

export function withDefaults(configuration: IPlayerConfiguration): IPlayerConfiguration {
  return {
    account: defaultValue(configuration.account, null),
    streamName: defaultValue(configuration.streamName, null),
    validationURL: defaultValue(configuration.validationURL, null),
    videoElementId: defaultValue(configuration.videoElementId, "llnw-rts-subscriber"),
    autoLayoutOrientation: defaultValue(configuration.autoLayoutOrientation, true),
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
