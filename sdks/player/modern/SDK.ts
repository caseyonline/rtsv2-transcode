import IPlayer from "./frontend/IPlayer";
import IPlayerConfiguration from "./frontend/IPlayerConfiguration";

import * as PlayerConfiguration from "./backend/PlayerConfiguration";
import Player from "./backend/Player";

// TODO: put the actual DNS name in here
const DOMAIN_NAME: string = "rts.llnwi.net";
const API_VERSION: string = "1.0";

// TODO
// const VALIDATION_URL = "https://subscribe-validator.rts.llnwi.net/mmddev001/auth/v2/llnw-test-001/?ci=100&cd=100&cf=1600000000&h=c0518fc5aa957b128147545dfe45cc0f";
const VALIDATION_URL = "http://172.16.171.1:3001/system/llnwstub/rts/v1/validation/valid";


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
  return new Player(configuration.account, configuration.streamName, socketURL, VALIDATION_URL, videoElement);
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
