// Set up the peerconnection interceptor and attach it 
// 
class RTCPeerConnectionInterceptor extends RTCPeerConnection {
  constructor(...args) {
    super(...args);
    window.RTSv2Peer = this
  }
}

window.RTCPeerConnection = RTCPeerConnectionInterceptor;
let RTSv2Peer = new RTCPeerConnectionInterceptor

// get the JSON for a specific video channel
function videoLoaded() {
  window.RTSv2Peer.getStats(null).then(stats => {
    stats.forEach(report => {
        if (report.kind == "video" && report.type == "inbound-rtp") {
          console.log(report)
        }
      })     
    }); 
}

function waitForDom(callbackFunction){
  if(document.readyState != 'loading')
    callbackFunction(event)
  else
    document.addEventListener("DOMContentLoaded", callbackFunction)
}

waitForDom(event => {
  let videoElem = document.getElementById("llnw-rts-subscriber")
  videoElem.addEventListener('playing', videoLoaded ,false);
  
});
