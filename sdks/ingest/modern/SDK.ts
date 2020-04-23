import { IIngest } from "./frontend/IIngest";
import IIngestConfiguration from "./frontend/IIngestConfiguration";

import * as IngestConfiguration from "./backend/IngestConfiguration";
import Ingest from "./backend/Ingest";

// TODO: put the actual DNS name in here
const DOMAIN_NAME: string = "rts.llnwi.net";
const API_VERSION: string = "1.0";

export function createIngest(configuration: IIngestConfiguration) {
  const configurationWithDefaults = IngestConfiguration.withDefaults(configuration);
  return createIngestPrime(configurationWithDefaults);
}

function createIngestPrime(configuration: IIngestConfiguration) {

  // NOTE: we pack all the info into the URI because that allows the edge
  //       to do an HTTP-level redirect if it wants us to go somewhere else
  const socketURL = socketUrl(configuration);

  const videoElement =
    <HTMLVideoElement>document.getElementById(configuration.videoElementId);

  // TODO: Preflight checks?
  return new Ingest(configuration.account, configuration.streamName, socketURL, videoElement);
}

function getOrCreateTraceId(): string {
  return null;
}

// TODO - currently patch requires override since the thing below does not reflect reality
function socketUrl(configuration: IIngestConfiguration) {
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
