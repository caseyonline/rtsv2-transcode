class RTCPeerConnectionInterceptor extends RTCPeerConnection {
  constructor(...args) {
      super(...args);
      console.log("Created a new peer connection...", this);
    window.pc = this;
  }
  async getNiceStats () {
    const stats = await this.getStats();
    let result = [];
    for (let stat of stats.entries()) {
      result.push(stat);
    }
    return result
  }
}

window.RTCPeerConnection = RTCPeerConnectionInterceptor;

function ready(callbackFunction){
  if(document.readyState != 'loading')
    callbackFunction(event)
  else
    document.addEventListener("DOMContentLoaded", callbackFunction)
}

ready(event => {
  console.log('DOM is ready.')
//   Display some of the stats into a box in the DOM
//   
//   let div = document.createElement("div");
//   div.id = "stats-box"
//   document.getElementById("container").appendChild(div)
  
//   window.setInterval(function() {
//     window.pc.getStats(null).then(stats => {
      
//     let statsOutput = "";

//       stats.forEach(report => {
//         if (report.kind == "video") {

//           statsOutput += `<h2>Report: ${report.type}</h3>\n<strong>ID:</strong> ${report.id}<br>\n` +
//                      `<strong>Timestamp:</strong> ${report.timestamp}<br>\n`;
      
//       // Now the statistics for this report; we intentially drop the ones we
//       // sorted to the top above

//       Object.keys(report).forEach(statName => {
//         if (statName !== "id" && statName !== "timestamp" && statName !== "type") {
//           statsOutput += `<strong>${statName}:</strong> ${report[statName]}<br>\n`;
//         }
//       });
//         }
      
//     });

//     document.querySelector("#stats-box").innerHTML = statsOutput;
//   });
// }, 1000);
})

