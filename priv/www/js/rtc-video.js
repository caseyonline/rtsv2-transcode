import { Session } from "./lib/session.js";

const template = document.createElement('template');

template.innerHTML = `
<style>
  :host {
    display: block;
  }

  video {
    width: 400px;
  }
</style>
<video controls muted>
</video>
`;

class RTCVideo extends HTMLElement {
  constructor() {
    super();
    this._shadowRoot = this.attachShadow({ 'mode': 'open' });
    this._shadowRoot.appendChild(template.content.cloneNode( /* deep */ true));
    this._video = this._shadowRoot.querySelector('video');
    this._session = null;
  }

  connectedCallback() {
    this._maybeStartPlayback();
  }

  disconnectedCallback() {
    this._stopPlayback();
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if ( name === "src" ) {
      console.debug(`Video source changed from ${oldVal} to ${newVal}.`);
      this._maybeStartPlayback();
    }
  }

  adoptedCallback() {
    console.log("adopted!");
  }

  readyState() {
    return this._video.readyState;
  }

  startTime() {
    return this._startTime;
  }

  _maybeStartPlayback() {
    this._stopPlayback();

    const src = this.getAttribute("src");

    if (!src) {
      return;
    }

    console.debug(`Starting video playback with src ${src}.`);
    this._session = new Session(src, this._video);
    this._session.start();
    this._startTime = new Date();
  }

  _stopPlayback() {
    if ( this._session === null )  {
      return;
    }

    console.debug("Stopping video playback.");
    this._session.stop();
    this._session = null;
  }
}

window.customElements.define("rtc-video", RTCVideo);
