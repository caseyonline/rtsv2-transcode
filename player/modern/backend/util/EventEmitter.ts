const listenersSymbol = Symbol("listeners");

export default class EventEmitter {
  constructor() {
    this[listenersSymbol] = new Map();
  }

  on(eventName, listener) {
    const eventListeners = this[listenersSymbol].get(eventName) || [];

    eventListeners.push(listener);

    this[listenersSymbol].set(eventName, eventListeners);
  }

  emit(eventName, ...eventArgs) {
    const eventListeners = this[listenersSymbol].get(eventName) || [];

    for ( let listener of eventListeners ) {
      listener(...eventArgs);
    }
  }
}
