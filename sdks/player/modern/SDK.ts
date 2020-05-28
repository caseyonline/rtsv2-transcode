import IPlayer from "./frontend/IPlayer";
import IPlayerConfiguration from "./frontend/IPlayerConfiguration";

import * as PlayerConfiguration from "./backend/PlayerConfiguration";
import Player from "./backend/Player";

// TODO: put the actual DNS name in here
const DOMAIN_NAME: string = "rts.llnwi.net";
const API_VERSION: string = "1.0";

export function createPlayer(configuration: IPlayerConfiguration) {
  const configurationWithDefaults = PlayerConfiguration.withDefaults(configuration);
  return createPlayerPrime(configurationWithDefaults);
}

function createPlayerPrime(configuration: IPlayerConfiguration) {

  // NOTE: we pack all the info into the URI because that allows the edge
  //       to do an HTTP-level redirect if it wants us to go somewhere else
  const socketURL = socketUrl(configuration);

  const videoElement =
    <HTMLVideoElement>document.getElementById(configuration.videoElementId);

  // TODO: Preflight checks?
  return new Player(configuration.account, configuration.streamName, socketURL, configuration.validationURL, videoElement);
}

function getOrCreateTraceId(): string {
  return null;
}

function socketUrl(configuration: IPlayerConfiguration) {
  const overrides = configuration.overrides || {};
  const authority = overrides.socketAuthority || `${configuration.account}.${DOMAIN_NAME}`;
  const scheme = socketScheme(overrides.socketSecure);
  const path = overrides.socketPath || `play/${API_VERSION}/${configuration.account}/${configuration.streamName}`;

  return `${scheme}://${authority}/${path}`;
}

function socketScheme(secureOption) {
  switch (secureOption) {
    case false:
      return "ws";
    default:
      return "wss";
  }
}
