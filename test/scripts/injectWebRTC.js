// --------------------- RTCPeerConnection ---------------------------
// Set up the peerconnection interceptor and attach it
//
class RTCPeerConnectionInterceptor extends RTCPeerConnection {
  constructor(...args) {
    super(...args);
    window.RTSv2Peer = this
  }
}
// overide the browser RTCPeerConnection
window.RTCPeerConnection = RTCPeerConnectionInterceptor;

// --------------------- VIDEO ---------------------------
// get the JSON for a specific video channel
function getVideoStats() {
  return window.RTSv2Peer.getStats(null).then(stats => {
    let results = {}
    stats.forEach(report => {
      if (report.kind == "video" && report.type == "inbound-rtp") {
        result = report
      }
    })
    return results
  });
}

// put our function onto the window
window.getVideoStats = getVideoStats

// --------------------- AUDIO ---------------------------
// get the JSON for a specific video channel
function getAudioStats() {
  return window.RTSv2Peer.getStats(null).then(stats => {
    let results = {}
    stats.forEach(report => {
      if (report.kind == "audio" && report.type == "track") {
        results = report
      }
    })
    return results
  });
}

// put our function onto the window
window.getAudioStats = getAudioStats
