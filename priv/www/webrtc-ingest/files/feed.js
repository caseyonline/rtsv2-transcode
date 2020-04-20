import Ingest from "./Ingest.js";

function getSocketPath() {
  const hostAndPort = window.location.host;
  const path = window.location.pathname.match("^(.*/)")[0];

  switch (window.location.protocol) {
  case "http:":
    return "ws://" + hostAndPort + path + "control";
  case "https:":
    return "ws://" + hostAndPort + path + "control";
  }
}

function getAccount() {
  const parts = window.location.pathname.split('/');
  return parts[parts.length - 3];
}

function getStreamName() {
  const parts = window.location.pathname.split('/');
  return parts[parts.length - 2];
}

export const socketPath = getSocketPath();
export const account = getAccount();
export const streamName = getStreamName();
export const ingest = new Ingest(socketPath);

const callButton = document.getElementById("start-ingest");

ingest.on("starting", function onStarting() {
});

ingest.on("startFailed", function onStartFailed(error) {
  callButton.disabled = false;
});

ingest.on("started", function() {
  console.log("Obtained source, connecting over WebRTC...");
  ingest.call();
});

ingest.on("called", function() {
  console.log("Call succeeded.");
});

ingest.on("hangup", function() {
  console.log("Call disconnected.");
});


callButton.addEventListener("click", () => {
  const username = document.getElementById("username").value;
  const password = document.getElementById("password").value;

  callButton.disabled = true;
  ingest.start(username, password);
}, false);
