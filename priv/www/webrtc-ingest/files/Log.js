export default class Log {
  constructor(prefix) {
    this.prefix = prefix;
    this.emitDebug = true;
    this.emitInfo = true;
    this.emitWarning = true;
    this.emitError =true;
  }

  info(...args) {
    if (!this.emitInfo) {
      return;
    }
    this.log("[info]: ", ...args);
  }

  debug(...args) {
    if (!this.emitDebug) {
      return;
    }
    this.log("[debug]: ", ...args);
  }

  warning(...args) {
    if (!this.emitWarning) {
      return;
    }
    this.log("[warning]: ", ...args);
  }

  error(...args) {
    if (!this.emitError) {
      return;
    }
    this.log("[error]: ", ...args);
  }

  log(type, msg, ...args) {
    console.log(this.prefix + type + msg, ...args);
  }
}
