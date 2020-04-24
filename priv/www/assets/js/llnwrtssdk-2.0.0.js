// [LLNW RTS SDK]  v2.0.0 built 2020-04-24T14:46:02+0100  
 (function e(t,n){"object"===typeof exports&&"object"===typeof module?module.exports=n():"function"===typeof define&&define.amd?define([],n):"object"===typeof exports?exports["LimelightSDK"]=n():t["LimelightSDK"]=n()})("undefined"!==typeof self?self:this,function(){return function(e){var t={};function n(o){if(t[o])return t[o].exports;var i=t[o]={i:o,l:false,exports:{}};e[o].call(i.exports,i,i.exports,n);i.l=true;return i.exports}n.m=e;n.c=t;n.d=function(e,t,o){n.o(e,t)||Object.defineProperty(e,t,{configurable:false,enumerable:true,get:o})};n.n=function(e){var t=e&&e.__esModule?function t(){return e["default"]}:function t(){return e};n.d(t,"a",t);return t};n.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)};n.p="";return n(n.s=1)}([function(e,t,n){"use strict";var o=Symbol("listeners");var i=function(){function e(){this[o]=new Map}e.prototype.on=function(e,t){var n=this[o].get(e)||[];n.push(t);this[o].set(e,n)};e.prototype.emit=function(e){var t=[];for(var n=1;n<arguments.length;n++)t[n-1]=arguments[n];var i=this[o].get(e)||[];for(var s=0,a=i;s<a.length;s++){var r=a[s];r.apply(void 0,t)}};return e}();t["a"]=i},function(e,t,n){"use strict";Object.defineProperty(t,"__esModule",{value:true});t["createPlayer"]=r;var o=n(2);var i=n(3);var s="rts.llnwi.net";var a="1.0";function r(e){var t=o["a"](e);return c(t)}function c(e){var t=l(e);var n=document.getElementById(e.videoElementId);return new i["a"](e.account,e.streamName,t,n)}function u(){return null}function l(e){var t=e.overrides||{};var n=t.socketAuthority||e.account+"."+s;var o=d(t.socketSecure);var i=t.socketPath||"play/"+a+"/"+e.account+"/"+e.streamName;return o+"://"+n+"/"+i}function d(e){switch(e){case false:return"ws";default:return"wss"}}},function(e,t,n){"use strict";t["a"]=o;function o(e){return{account:i(e.account,null),streamName:i(e.streamName,null),videoElementId:i(e.videoElementId,"llnw-rts-subscriber"),autoLayoutOrientation:i(e.autoLayoutOrientation,true),overrides:s(i(e.overrides,{}))}}function i(e,t){return void 0===e||null===e?t:e}function s(e){return{socketAuthority:i(e.socketAuthority,null),socketSecure:i(e.socketSecure,null),socketPath:i(e.socketPath,null)}}},function(e,t,n){"use strict";var o=n(4);var i=n(5);var s=n(0);var a=this&&this.__extends||function(){var e=function(t,n){e=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var n in t)t.hasOwnProperty(n)&&(e[n]=t[n])};return e(t,n)};return function(t,n){e(t,n);function o(){this.constructor=t}t.prototype=null===n?Object.create(n):(o.prototype=n.prototype,new o)}}();var r=function(e){a(t,e);function t(t,n,i,s){var a=e.call(this)||this;a.localStream=null;a.account=t;a.stream=n;a.videoElement=s;a.session=new o["a"](i);a.session.on("stream",function(e){console.debug("Received stream from session manager.");a.videoElement.srcObject=e;a.localStream=e});a.session.on("quality-change",function(e){a.emit("quality-change",e)});a.session.on("on-fi",function(e){a.emit("on-fi",e)});a.session.on("active-profiles",function(e){a.emit("active-profiles",e)});a.session.on("data-object-message",function(e){a.emit("data-object-message",e)});a.session.on("data-object-update-response",function(e){a.emit("dataobject.update-response",e)});a.session.on("data-object",function(e){a.emit("data-object",e)});a.session.on("playback-active",function(){a.emit("playback-active",{})});a.session.on("playback-audio-stats",function(e){a.emit("playback-audio-stats",e)});a.session.on("playback-video-stats",function(e){a.emit("playback-video-stats",e)});return a}t.prototype.stop=function(){this.session.stop();this.videoElement.srcObject=null;this.videoElement.currentTime=0;this.localStream&&this.localStream.getTracks().forEach(function(e){return e.stop()})};t.prototype.requestMigrate=function(e){this.session.requestMigrate(e)};t.prototype.setProfile=function(e){this.session.setQualityConstraint({behavior:i["a"].ForceQuality,variant:e})};t.prototype.sendPrivateMessage=function(e,t){this.session.sendMessage({tag:"private",to:e},t)};t.prototype.sendPublisherMessage=function(e){this.session.sendMessage({tag:"publisher"},e)};t.prototype.sendBroadcastMessage=function(e){this.session.sendMessage({tag:"broadcast"},e)};t.prototype.dataObjectInc=function(e,t,n,o){this.session.sendUpdate({tag:"inc",keys:e,increment:t,createIfKeyMissing:o},n)};t.prototype.dataObjectDec=function(e,t,n,o){this.session.sendUpdate({tag:"dec",keys:e,decrement:t,createIfKeyMissing:o},n)};t.prototype.dataObjectCAS=function(e,t,n,o,i){this.session.sendUpdate({tag:"cas",keys:e,compare:t,swap:n,createIfKeyMissing:o},i)};t.prototype.dataObjectAdd=function(e,t,n,o){this.session.sendUpdate({tag:"add",keys:e,value:t,failIfKeyPresent:n},o)};t.prototype.dataObjectUpdate=function(e,t,n,o){this.session.sendUpdate({tag:"update",keys:e,value:t,createIfKeyMissing:n},o)};t.prototype.dataObjectDelete=function(e,t,n){this.session.sendUpdate({tag:"delete",keys:e,failIfKeyMissing:t},n)};t.prototype.dataObjectListInsert=function(e,t,n,o,i){this.session.sendUpdate({tag:"list.insert",keys:e,value:t,createIfKeyMissing:n,failIfValuePresent:o},i)};t.prototype.dataObjectListRemove=function(e,t,n,o,i){this.session.sendUpdate({tag:"list.remove",keys:e,value:t,failIfKeyMissing:n,failIfValueMissing:o},i)};return t}(s["a"]);t["a"]=r},function(e,t,n){"use strict";var o=n(0);var i=this&&this.__extends||function(){var e=function(t,n){e=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var n in t)t.hasOwnProperty(n)&&(e[n]=t[n])};return e(t,n)};return function(t,n){e(t,n);function o(){this.constructor=t}t.prototype=null===n?Object.create(n):(o.prototype=n.prototype,new o)}}();var s=this&&this.__awaiter||function(e,t,n,o){function i(e){return e instanceof n?e:new n(function(t){t(e)})}return new(n||(n=Promise))(function(n,s){function a(e){try{c(o.next(e))}catch(e){s(e)}}function r(e){try{c(o["throw"](e))}catch(e){s(e)}}function c(e){e.done?n(e.value):i(e.value).then(a,r)}c((o=o.apply(e,t||[])).next())})};var a=this&&this.__generator||function(e,t){var n={label:0,sent:function(){if(1&s[0])throw s[1];return s[1]},trys:[],ops:[]},o,i,s,a;return a={next:r(0),throw:r(1),return:r(2)},"function"===typeof Symbol&&(a[Symbol.iterator]=function(){return this}),a;function r(e){return function(t){return c([e,t])}}function c(a){if(o)throw new TypeError("Generator is already executing.");while(n)try{if(o=1,i&&(s=2&a[0]?i["return"]:a[0]?i["throw"]||((s=i["return"])&&s.call(i),0):i.next)&&!(s=s.call(i,a[1])).done)return s;(i=0,s)&&(a=[2&a[0],s.value]);switch(a[0]){case 0:case 1:s=a;break;case 4:n.label++;return{value:a[1],done:false};case 5:n.label++;i=a[1];a=[0];continue;case 7:a=n.ops.pop();n.trys.pop();continue;default:if(!(s=n.trys,s=s.length>0&&s[s.length-1])&&(6===a[0]||2===a[0])){n=0;continue}if(3===a[0]&&(!s||a[1]>s[0]&&a[1]<s[3])){n.label=a[1];break}if(6===a[0]&&n.label<s[1]){n.label=s[1];s=a;break}if(s&&n.label<s[2]){n.label=s[2];n.ops.push(a);break}s[2]&&n.ops.pop();n.trys.pop();continue}a=t.call(e,n)}catch(e){a=[6,e];i=0}finally{o=s=0}if(5&a[0])throw a[1];return{value:a[0]?a[1]:void 0,done:true}}};var r;(function(e){e[e["Opening"]=1e3]="Opening";e[e["AwaitingInitialization"]=2e3]="AwaitingInitialization";e[e["Negotiating"]=3e3]="Negotiating"})(r||(r={}));var c=15e3;var u=function(e){i(t,e);function t(t){var n=e.call(this)||this;n.socket=null;n.state=r.Opening;n.traceId=null;n.serverConfig=null;n.peer=null;n.peerCanReconfigure="setConfiguration"in RTCPeerConnection.prototype;n.socketURL=t;n.createSocket();setInterval(function(){return n.pingSocket()},c);return n}t.prototype.createSocket=function(){var e=this;var t=this.socket;this.socket=new WebSocket(this.socketURL);this.socket.onopen=function(n){e.handleSocketOpen(n);if(null!==t){t.onopen=null;t.onclose=null;t.onerror=null;t.onmessage=null;t.close()}};this.socket.onclose=function(t){return e.handleSocketClose(t)};this.socket.onerror=function(t){return e.handleSocketError(t)};this.socket.onmessage=function(t){return e.handleSocketMessage(t)}};t.prototype.stop=function(){this.socket.onopen=null;this.socket.onclose=null;this.socket.onerror=null;this.socket.onmessage=null;this.peer.onicecandidate=null;this.peer.onicegatheringstatechange=null;this.peer.oniceconnectionstatechange=null;this.peer.onconnectionstatechange=null;this.peer.onsignalingstatechange=null;this.peer.ontrack=null;this.socket.close();this.peer.close()};t.prototype.pingSocket=function(){if(this.state===r.Opening)return;if(this.state===r.AwaitingInitialization)return;this.sendToSocket({type:"ping"})};t.prototype.setQualityConstraints=function(e){};t.prototype.handleSocketOpen=function(e){console.log("Opened WebSocket to "+this.socketURL+". Waiting for initialization message.");this.state=r.AwaitingInitialization};t.prototype.reconnectSocket=function(){this.createSocket()};t.prototype.switchServer=function(){};t.prototype.createDeferredLogEntry=function(e){};t.prototype.handleSocketClose=function(e){console.warn("The socket closed with code "+e.code+" and reason '"+e.reason+"'");switch(e.code){case 1e3:this.reconnectSocket();break;case 1001:this.switchServer();break;case 1006:this.switchServer();break;case 1011:this.reconnectSocket();break;case 1015:this.createDeferredLogEntry({message:"TLS failure.",socketURL:this.socketURL});this.switchServer();break;case 4e3:break;default:this.createDeferredLogEntry({message:"Unknown socket close status.",socketURL:this.socketURL,status:e.code,reason:e.reason});this.switchServer();break}};t.prototype.handleSocketError=function(e){console.error("The socket closed due to an error.",e)};t.prototype.handleSocketMessage=function(e){switch(this.state){case r.AwaitingInitialization:this.init_handleSocketMessage(e);break;case r.Negotiating:this.negotiating_handleSocketMessage(e);break}};t.prototype.init_handleSocketMessage=function(e){var t=JSON.parse(e.data);switch(t.type){case"init":var n=t.thisEdge;this.state=r.Negotiating;this.traceId=t.traceId;this.serverConfig={iceTransportPolicy:"all",iceServers:n.iceServers,iceCandidatePoolSize:2};console.log("Initialized Session with identifier "+t.traceId+", moved to state "+r[this.state]+" ("+this.state+"). Final endpoint: "+n.socketURL,n);this.beginNegotiation();break;case"bye":if(t.otherEdges.length>0){this.socketURL=t.otherEdges[0].socketURL;console.log("Rejected by server, we're being redirected to "+this.socketURL+".");this.createSocket()}else console.log("Rejected by server, no alternatives were provided.");break;default:this.unexpectedMessage(t)}};t.prototype.negotiating_handleSocketMessage=function(e){return s(this,void 0,void 0,function(){var t,n;return a(this,function(o){switch(o.label){case 0:t=JSON.parse(e.data);n=t.type;switch(n){case"pong":return[3,1];case"sdp.offer-response":return[3,2];case"ice.candidate":return[3,4];case"quality-change":return[3,6];case"on-fi":return[3,7];case"active-profiles":return[3,8];case"dataobject.message":return[3,9];case"dataobject.update-response":return[3,10];case"dataobject.broadcast":return[3,11]}return[3,12];case 1:return[3,13];case 2:console.debug("Remote description obtained.");return[4,this.peer.setRemoteDescription({sdp:t.response,type:"answer"})];case 3:o.sent();console.debug("Remote description applied.");return[3,13];case 4:console.debug("Remote ICE candidate obtained.",t);return[4,this.peer.addIceCandidate(new RTCIceCandidate({sdpMLineIndex:t.index,candidate:t.candidate}))];case 5:o.sent();console.debug("Remote ICE candidate applied.");return[3,13];case 6:this.emit("quality-change",t);return[3,13];case 7:this.emit("source-onfi",t);return[3,13];case 8:this.emit("active-profiles",t.activeProfiles);return[3,13];case 9:this.emit("data-object-message",t);return[3,13];case 10:this.emit("data-object-update-response",t);return[3,13];case 11:this.emit("data-object",t.object);return[3,13];case 12:this.unexpectedMessage(t);o.label=13;case 13:return[2]}})})};t.prototype.unexpectedMessage=function(e){console.error("Got unexpected message with type "+e.type+" in state "+this.state+".",e)};t.prototype.setQualityConstraint=function(e){this.sendToSocket({type:"set-quality-constraint-configuration",configuration:e})};t.prototype.reportStats=function(){var e=this;if(this.state==r.Negotiating){this.peer.getReceivers().forEach(function(t){return t.getStats().then(function(n){e.emit("playback-"+t.track.kind+"-stats",n)})});setTimeout(function(){e.reportStats()},1e3)}};t.prototype.requestMigrate=function(e){console.debug("Attempting migration to "+e);this.socketURL=e;this.createSocket()};t.prototype.sendMessage=function(e,t){this.sendToSocket({type:"dataobject.send-message",destination:e,msg:t})};t.prototype.sendUpdate=function(e,t){this.sendToSocket({type:"dataobject.update",senderRef:t,operation:e})};t.prototype.beginNegotiation=function(){return s(this,void 0,void 0,function(){var e,t,n,o;var i=this;return a(this,function(s){switch(s.label){case 0:e=this.peer;t=false;if(null!==e&&this.peerCanReconfigure){console.debug("Reconfiguring existing peer.");e.setConfiguration(this.serverConfig);t=true}else{e=this.peer=new RTCPeerConnection(this.serverConfig);e.onicecandidate=function(e){return i.handlePeerICECandidate(e)};e.onicegatheringstatechange=function(e){return i.handlePeerICEGatheringStateChange(e)};e.oniceconnectionstatechange=function(e){return i.handlePeerICEConnectionStateChange(e)};e.onconnectionstatechange=function(e){return i.handlePeerConnectionStateChange(e)};e.onsignalingstatechange=function(e){return i.handlePeerSignalingStateChange(e)};e.ontrack=function(e){return i.handlePeerTrack(e)}}s.label=1;case 1:s.trys.push([1,4,,5]);return[4,e.createOffer({iceRestart:t,offerToReceiveAudio:true,offerToReceiveVideo:true})];case 2:n=s.sent();console.debug("Local description obtained.");this.sendToSocket({type:"sdp.offer",offer:n.sdp});console.debug("Local description sent to server.");return[4,e.setLocalDescription(n)];case 3:s.sent();console.debug("Local description applied.");return[3,5];case 4:o=s.sent();console.error("Something went less than spectacularly whilst configuring the peer.");return[3,5];case 5:return[2]}})})};t.prototype.handlePeerICECandidate=function(e){var t=e.candidate;if(null===t){console.log("Local ICE candidate gathering has completed.");this.sendToSocket({type:"ice.done"})}else{console.log("Local ICE candidate received for "+t.sdpMLineIndex+": '"+t.candidate+"'");this.sendToSocket({type:"ice.candidate",candidate:t.candidate,index:t.sdpMLineIndex})}};t.prototype.handlePeerICEGatheringStateChange=function(e){console.debug("ICE Gathering State changed to "+e.target.iceGatheringState)};t.prototype.handlePeerICEConnectionStateChange=function(e){console.debug("ICE Connection State changed to "+e.target.iceConnectionState)};t.prototype.handlePeerConnectionStateChange=function(e){var t=this;console.debug("Connection State changed to "+e.target.connectionState);switch(e.target.connectionState){case"connected":setTimeout(function(){t.reportStats()},1e3);this.emit("playback-active",{});break}};t.prototype.handlePeerSignalingStateChange=function(e){console.debug("Signaling State changed to "+e.target.signalingState)};t.prototype.handlePeerTrack=function(e){console.debug("Got track",e.track);this.emit("stream",e.streams[0])};t.prototype.sendToSocket=function(e){try{this.socket.send(JSON.stringify(e))}catch(e){switch(e.name){case"InvalidStateError":console.error("Trying to send a message whilst the socket isn't open.");break;case"SyntaxError":console.error("Trying to send a message containing a syntax error.");break;default:console.error("Unexpected error sending a message on the socket.");break}}};return t}(o["a"]);t["a"]=u},function(e,t,n){"use strict";n.d(t,"a",function(){return o});var o;(function(e){e["ForceQuality"]="force-quality";e["MaxQuality"]="max-quality"})(o||(o={}))}])}); 