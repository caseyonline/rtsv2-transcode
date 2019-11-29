var puppeteer = require("puppeteer");

// --------------------- VIDEO ---------------------------
// expose getVideoStats to FFI
exports._getVideoStats = function(page) {
  return function() {
    return page.evaluate(function() {
      return window.getVideoStats();
    });
  }
};

// --------------------- AUDIO ---------------------------
// expose getAudioStats to FFI
exports._getAudioStats = function(page) {
  return function() {
    return page.evaluate(function() {
      return window.getAudioStats();
    });
  }
};

// this might be usefull in the future
// --------------------- INIT DOM ---------------------------
// wait for the Dom to be loaded
// exports._waitForDOMthenRun = function(callbackFunction){
//   if(document.readyState != 'loading')
//     callbackFunction(event)
//   else
//     document.addEventListener
//       ("DOMContentLoaded",
//         (event) => {
//           let videoElem = document.getElementById("llnw-rts-subscriber")
//           videoElem.addEventListener('playing', callbackFunction ,false);
//         }
//       )
// }
