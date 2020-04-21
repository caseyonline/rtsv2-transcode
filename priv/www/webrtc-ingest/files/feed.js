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

const startButton = document.getElementById("start-ingest");
const stopButton = document.getElementById("stop-ingest");

ingest.on("starting", function onStarting() {
  console.log("Ingest starting");
});

ingest.on("startFailed", function onStartFailed(error) {
  startButton.disabled = false;
});

ingest.on("started", function() {
  console.log("Obtained source, connecting over WebRTC...");
  ingest.call();
});

ingest.on("called", function() {
  console.log("Call succeeded.");

  ingest.rtc.localStream.getTracks().forEach((track) => {
    if (track.kind == "video") {
      document.getElementById("camera").innerHTML = track.label;
    }
    else if (track.kind == "audio") {
      document.getElementById("microphone").innerHTML = track.label;
    }
  });

  setTimeout(() => displayStats(), 1000);
});

ingest.on("hangup", function() {
  console.log("Call disconnected.");
  ingest.stop();
});

ingest.on("stop", function() {
  console.log("Socket closed.");
  ingest = new Ingest(socketPath);
});

startButton.addEventListener("click", () => {
  const username = document.getElementById("username").value;
  const password = document.getElementById("password").value;

  startButton.disabled = true;
  stopButton.disabled = false;
  ingest.start(username, password, 5000);
  window.ingest = ingest;
}, false);

stopButton.addEventListener("click", () => {
  startButton.disabled = false;
  stopButton.disabled = true;
  ingest.hangup();
  window.ingest = undefined;
}, false);

function displayStats() {

  ingest.rtc.client.getSenders().forEach((sender) => {
    if (sender.track.kind == "video") {
      sender.getStats()
        .then( (stats) => {
          for (let stat of stats.values()) {
            switch (stat.type) {
            case "codec": {
              console.log("timestamp", stat.timestamp);
              break;
            }
            case "outbound-rtp": {
              console.log("bytesSent", stat.bytesSent);
              console.log("frames", stat.framesEncoded);
              console.log("keyframes", stat.keyFramesEncoded);
              console.log("packets", stat.packetsSent);
              console.log("pli", stat.pliCount);
              console.log("sendDelay", stat.totalPacketSendDelay);
              break;
            }
            case "media-source": {
              console.log("fps", stat.framesPerSecond);
              console.log("height", stat.height);
              console.log("width", stat.width);
              break;
            }
            }
            console.log(stat);
          }
        });
    }
  });

  ingest.rtc.client.getStats()
    .then( (stats) => {
      document.getElementById("bytes-sent").innerHTML = stats.get("RTCTransport_0_1").bytesSent
    })

  setTimeout(() => displayStats(), 1000);
}


/*
a=fmtp:100 x-google-start-bitrate=4000; x-google-max-bitrate=5000; x-google-min-bitrate=3000

params = ingest.rtc.client.getSenders()[1].getParameters()
params.encodings[0].maxBitrate = 100000
ingest.rtc.client.getSenders()[1].setParameters(params)
*/
