// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"../../../../../.nvm/versions/node/v13.5.0/lib/node_modules/parcel-bundler/node_modules/process/browser.js":[function(require,module,exports) {

// shim for using process in browser
var process = module.exports = {}; // cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
  throw new Error('setTimeout has not been defined');
}

function defaultClearTimeout() {
  throw new Error('clearTimeout has not been defined');
}

(function () {
  try {
    if (typeof setTimeout === 'function') {
      cachedSetTimeout = setTimeout;
    } else {
      cachedSetTimeout = defaultSetTimout;
    }
  } catch (e) {
    cachedSetTimeout = defaultSetTimout;
  }

  try {
    if (typeof clearTimeout === 'function') {
      cachedClearTimeout = clearTimeout;
    } else {
      cachedClearTimeout = defaultClearTimeout;
    }
  } catch (e) {
    cachedClearTimeout = defaultClearTimeout;
  }
})();

function runTimeout(fun) {
  if (cachedSetTimeout === setTimeout) {
    //normal enviroments in sane situations
    return setTimeout(fun, 0);
  } // if setTimeout wasn't available but was latter defined


  if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
    cachedSetTimeout = setTimeout;
    return setTimeout(fun, 0);
  }

  try {
    // when when somebody has screwed with setTimeout but no I.E. maddness
    return cachedSetTimeout(fun, 0);
  } catch (e) {
    try {
      // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
      return cachedSetTimeout.call(null, fun, 0);
    } catch (e) {
      // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
      return cachedSetTimeout.call(this, fun, 0);
    }
  }
}

function runClearTimeout(marker) {
  if (cachedClearTimeout === clearTimeout) {
    //normal enviroments in sane situations
    return clearTimeout(marker);
  } // if clearTimeout wasn't available but was latter defined


  if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
    cachedClearTimeout = clearTimeout;
    return clearTimeout(marker);
  }

  try {
    // when when somebody has screwed with setTimeout but no I.E. maddness
    return cachedClearTimeout(marker);
  } catch (e) {
    try {
      // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
      return cachedClearTimeout.call(null, marker);
    } catch (e) {
      // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
      // Some versions of I.E. have different rules for clearTimeout vs setTimeout
      return cachedClearTimeout.call(this, marker);
    }
  }
}

var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
  if (!draining || !currentQueue) {
    return;
  }

  draining = false;

  if (currentQueue.length) {
    queue = currentQueue.concat(queue);
  } else {
    queueIndex = -1;
  }

  if (queue.length) {
    drainQueue();
  }
}

function drainQueue() {
  if (draining) {
    return;
  }

  var timeout = runTimeout(cleanUpNextTick);
  draining = true;
  var len = queue.length;

  while (len) {
    currentQueue = queue;
    queue = [];

    while (++queueIndex < len) {
      if (currentQueue) {
        currentQueue[queueIndex].run();
      }
    }

    queueIndex = -1;
    len = queue.length;
  }

  currentQueue = null;
  draining = false;
  runClearTimeout(timeout);
}

process.nextTick = function (fun) {
  var args = new Array(arguments.length - 1);

  if (arguments.length > 1) {
    for (var i = 1; i < arguments.length; i++) {
      args[i - 1] = arguments[i];
    }
  }

  queue.push(new Item(fun, args));

  if (queue.length === 1 && !draining) {
    runTimeout(drainQueue);
  }
}; // v8 likes predictible objects


function Item(fun, array) {
  this.fun = fun;
  this.array = array;
}

Item.prototype.run = function () {
  this.fun.apply(null, this.array);
};

process.title = 'browser';
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues

process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) {
  return [];
};

process.binding = function (name) {
  throw new Error('process.binding is not supported');
};

process.cwd = function () {
  return '/';
};

process.chdir = function (dir) {
  throw new Error('process.chdir is not supported');
};

process.umask = function () {
  return 0;
};
},{}],"rtsv2AppBundle.js":[function(require,module,exports) {
var process = require("process");
function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

// Generated by purs bundle 0.13.5
var PS = {};

(function (exports) {
  /* global exports */

  /* global XMLHttpRequest */

  /* global module */

  /* global process */
  "use strict";

  exports._ajax = function () {
    var platformSpecific = {};

    if (typeof module !== "undefined" && module.require && !(typeof process !== "undefined" && process.versions["electron"])) {
      // We are on node.js
      platformSpecific.newXHR = function () {
        var XHR = module.require("xhr2");

        return new XHR();
      };

      platformSpecific.fixupUrl = function (url) {
        var urllib = module.require("url");

        var u = urllib.parse(url);
        u.protocol = u.protocol || "http:";
        u.hostname = u.hostname || "localhost";
        return urllib.format(u);
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    } else {
      // We are in the browser
      platformSpecific.newXHR = function () {
        return new XMLHttpRequest();
      };

      platformSpecific.fixupUrl = function (url) {
        return url || "/";
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    }

    return function (mkHeader, options) {
      return function (errback, callback) {
        var xhr = platformSpecific.newXHR();
        var fixedUrl = platformSpecific.fixupUrl(options.url);
        xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);

        if (options.headers) {
          try {
            for (var i = 0, header; (header = options.headers[i]) != null; i++) {
              xhr.setRequestHeader(header.field, header.value);
            }
          } catch (e) {
            errback(e);
          }
        }

        var onerror = function onerror(msg) {
          return function () {
            errback(new Error(msg + ": " + options.method + " " + options.url));
          };
        };

        xhr.onerror = onerror("AJAX request failed");
        xhr.ontimeout = onerror("AJAX request timed out");

        xhr.onload = function () {
          callback({
            status: xhr.status,
            statusText: xhr.statusText,
            headers: xhr.getAllResponseHeaders().split("\r\n").filter(function (header) {
              return header.length > 0;
            }).map(function (header) {
              var i = header.indexOf(":");
              return mkHeader(header.substring(0, i))(header.substring(i + 2));
            }),
            body: platformSpecific.getResponse(xhr)
          });
        };

        xhr.responseType = options.responseType;
        xhr.withCredentials = options.withCredentials;
        xhr.send(options.content);
        return function (error, cancelErrback, cancelCallback) {
          try {
            xhr.abort();
          } catch (e) {
            return cancelErrback(e);
          }

          return cancelCallback();
        };
      };
    };
  }();
})(PS["Affjax"] = PS["Affjax"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Alt"] = $PS["Control.Alt"] || {};
  var exports = $PS["Control.Alt"];

  var Alt = function Alt(Functor0, alt) {
    this.Functor0 = Functor0;
    this.alt = alt;
  };

  var alt = function alt(dict) {
    return dict.alt;
  };

  exports["Alt"] = Alt;
  exports["alt"] = alt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];

  var Semigroupoid = function Semigroupoid(compose) {
    this.compose = compose;
  };

  var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
      return function (x) {
        return f(g(x));
      };
    };
  });

  var compose = function compose(dict) {
    return dict.compose;
  };

  var composeFlipped = function composeFlipped(dictSemigroupoid) {
    return function (f) {
      return function (g) {
        return compose(dictSemigroupoid)(g)(f);
      };
    };
  };

  exports["compose"] = compose;
  exports["Semigroupoid"] = Semigroupoid;
  exports["composeFlipped"] = composeFlipped;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];

  var Category = function Category(Semigroupoid0, identity) {
    this.Semigroupoid0 = Semigroupoid0;
    this.identity = identity;
  };

  var identity = function identity(dict) {
    return dict.identity;
  };

  var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
    return x;
  });
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];

  var on = function on(f) {
    return function (g) {
      return function (x) {
        return function (y) {
          return f(g(x))(g(y));
        };
      };
    };
  };

  var flip = function flip(f) {
    return function (b) {
      return function (a) {
        return f(a)(b);
      };
    };
  };

  var $$const = function $$const(a) {
    return function (v) {
      return a;
    };
  };

  var applyFlipped = function applyFlipped(x) {
    return function (f) {
      return f(x);
    };
  };

  exports["flip"] = flip;
  exports["const"] = $$const;
  exports["applyFlipped"] = applyFlipped;
  exports["on"] = on;
})(PS);

(function (exports) {
  "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);

      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }

      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});

(function (exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var $foreign = $PS["Data.Functor"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];

  var Functor = function Functor(map) {
    this.map = map;
  };

  var map = function map(dict) {
    return dict.map;
  };

  var mapFlipped = function mapFlipped(dictFunctor) {
    return function (fa) {
      return function (f) {
        return map(dictFunctor)(f)(fa);
      };
    };
  };

  var $$void = function $$void(dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };

  var voidLeft = function voidLeft(dictFunctor) {
    return function (f) {
      return function (x) {
        return map(dictFunctor)(Data_Function["const"](x))(f);
      };
    };
  };

  var voidRight = function voidRight(dictFunctor) {
    return function (x) {
      return map(dictFunctor)(Data_Function["const"](x));
    };
  };

  var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
  var functorArray = new Functor($foreign.arrayMap);

  var flap = function flap(dictFunctor) {
    return function (ff) {
      return function (x) {
        return map(dictFunctor)(function (f) {
          return f(x);
        })(ff);
      };
    };
  };

  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["mapFlipped"] = mapFlipped;
  exports["void"] = $$void;
  exports["voidRight"] = voidRight;
  exports["voidLeft"] = voidLeft;
  exports["flap"] = flap;
  exports["functorFn"] = functorFn;
  exports["functorArray"] = functorArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];

  var Apply = function Apply(Functor0, apply) {
    this.Functor0 = Functor0;
    this.apply = apply;
  };

  var applyFn = new Apply(function () {
    return Data_Functor.functorFn;
  }, function (f) {
    return function (g) {
      return function (x) {
        return f(x)(g(x));
      };
    };
  });

  var apply = function apply(dict) {
    return dict.apply;
  };

  var applyFirst = function applyFirst(dictApply) {
    return function (a) {
      return function (b) {
        return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"])(a))(b);
      };
    };
  };

  var applySecond = function applySecond(dictApply) {
    return function (a) {
      return function (b) {
        return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn)))(a))(b);
      };
    };
  };

  var lift2 = function lift2(dictApply) {
    return function (f) {
      return function (a) {
        return function (b) {
          return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b);
        };
      };
    };
  };

  exports["Apply"] = Apply;
  exports["apply"] = apply;
  exports["applyFirst"] = applyFirst;
  exports["applySecond"] = applySecond;
  exports["lift2"] = lift2;
  exports["applyFn"] = applyFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Unit = $PS["Data.Unit"];

  var Applicative = function Applicative(Apply0, pure) {
    this.Apply0 = Apply0;
    this.pure = pure;
  };

  var pure = function pure(dict) {
    return dict.pure;
  };

  var unless = function unless(dictApplicative) {
    return function (v) {
      return function (v1) {
        if (!v) {
          return v1;
        }

        ;

        if (v) {
          return pure(dictApplicative)(Data_Unit.unit);
        }

        ;
        throw new Error("Failed pattern match at Control.Applicative (line 62, column 1 - line 62, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };

  var when = function when(dictApplicative) {
    return function (v) {
      return function (v1) {
        if (v) {
          return v1;
        }

        ;

        if (!v) {
          return pure(dictApplicative)(Data_Unit.unit);
        }

        ;
        throw new Error("Failed pattern match at Control.Applicative (line 57, column 1 - line 57, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };

  var liftA1 = function liftA1(dictApplicative) {
    return function (f) {
      return function (a) {
        return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
      };
    };
  };

  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
  exports["unless"] = unless;
  exports["when"] = when;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];

  var Discard = function Discard(discard) {
    this.discard = discard;
  };

  var Bind = function Bind(Apply0, bind) {
    this.Apply0 = Apply0;
    this.bind = bind;
  };

  var discard = function discard(dict) {
    return dict.discard;
  };

  var bind = function bind(dict) {
    return dict.bind;
  };

  var bindFlipped = function bindFlipped(dictBind) {
    return Data_Function.flip(bind(dictBind));
  };

  var composeKleisliFlipped = function composeKleisliFlipped(dictBind) {
    return function (f) {
      return function (g) {
        return function (a) {
          return bindFlipped(dictBind)(f)(g(a));
        };
      };
    };
  };

  var composeKleisli = function composeKleisli(dictBind) {
    return function (f) {
      return function (g) {
        return function (a) {
          return bind(dictBind)(f(a))(g);
        };
      };
    };
  };

  var discardUnit = new Discard(function (dictBind) {
    return bind(dictBind);
  });

  var join = function join(dictBind) {
    return function (m) {
      return bind(dictBind)(m)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  exports["Bind"] = Bind;
  exports["bind"] = bind;
  exports["bindFlipped"] = bindFlipped;
  exports["discard"] = discard;
  exports["join"] = join;
  exports["composeKleisli"] = composeKleisli;
  exports["composeKleisliFlipped"] = composeKleisliFlipped;
  exports["discardUnit"] = discardUnit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Plus"] = $PS["Control.Plus"] || {};
  var exports = $PS["Control.Plus"];

  var Plus = function Plus(Alt0, empty) {
    this.Alt0 = Alt0;
    this.empty = empty;
  };

  var empty = function empty(dict) {
    return dict.empty;
  };

  exports["Plus"] = Plus;
  exports["empty"] = empty;
})(PS);

(function (exports) {
  "use strict";

  var refEq = function refEq(r1) {
    return function (r2) {
      return r1 === r2;
    };
  };

  exports.eqBooleanImpl = refEq;
  exports.eqIntImpl = refEq;
  exports.eqCharImpl = refEq;
  exports.eqStringImpl = refEq;
})(PS["Data.Eq"] = PS["Data.Eq"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Symbol"] = $PS["Data.Symbol"] || {};
  var exports = $PS["Data.Symbol"];

  var SProxy = function () {
    function SProxy() {}

    ;
    SProxy.value = new SProxy();
    return SProxy;
  }();

  var IsSymbol = function IsSymbol(reflectSymbol) {
    this.reflectSymbol = reflectSymbol;
  };

  var reflectSymbol = function reflectSymbol(dict) {
    return dict.reflectSymbol;
  };

  exports["IsSymbol"] = IsSymbol;
  exports["reflectSymbol"] = reflectSymbol;
  exports["SProxy"] = SProxy;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeHas = function (label) {
    return function (rec) {
      return {}.hasOwnProperty.call(rec, label);
    };
  };

  exports.unsafeGet = function (label) {
    return function (rec) {
      return rec[label];
    };
  };

  exports.unsafeSet = function (label) {
    return function (value) {
      return function (rec) {
        var copy = {};

        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy[key] = rec[key];
          }
        }

        copy[label] = value;
        return copy;
      };
    };
  };
})(PS["Record.Unsafe"] = PS["Record.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Record.Unsafe"] = $PS["Record.Unsafe"] || {};
  var exports = $PS["Record.Unsafe"];
  var $foreign = $PS["Record.Unsafe"];
  exports["unsafeHas"] = $foreign.unsafeHas;
  exports["unsafeGet"] = $foreign.unsafeGet;
  exports["unsafeSet"] = $foreign.unsafeSet;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Type.Data.RowList"] = $PS["Type.Data.RowList"] || {};
  var exports = $PS["Type.Data.RowList"];

  var RLProxy = function () {
    function RLProxy() {}

    ;
    RLProxy.value = new RLProxy();
    return RLProxy;
  }();

  exports["RLProxy"] = RLProxy;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Eq"] = $PS["Data.Eq"] || {};
  var exports = $PS["Data.Eq"];
  var $foreign = $PS["Data.Eq"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var EqRecord = function EqRecord(eqRecord) {
    this.eqRecord = eqRecord;
  };

  var Eq = function Eq(eq) {
    this.eq = eq;
  };

  var eqUnit = new Eq(function (v) {
    return function (v1) {
      return true;
    };
  });
  var eqString = new Eq($foreign.eqStringImpl);
  var eqRowNil = new EqRecord(function (v) {
    return function (v1) {
      return function (v2) {
        return true;
      };
    };
  });

  var eqRecord = function eqRecord(dict) {
    return dict.eqRecord;
  };

  var eqRec = function eqRec(dictRowToList) {
    return function (dictEqRecord) {
      return new Eq(eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value));
    };
  };

  var eqInt = new Eq($foreign.eqIntImpl);
  var eqChar = new Eq($foreign.eqCharImpl);
  var eqBoolean = new Eq($foreign.eqBooleanImpl);

  var eq = function eq(dict) {
    return dict.eq;
  };

  var eqRowCons = function eqRowCons(dictEqRecord) {
    return function (dictCons) {
      return function (dictIsSymbol) {
        return function (dictEq) {
          return new EqRecord(function (v) {
            return function (ra) {
              return function (rb) {
                var tail = eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                var get = Record_Unsafe.unsafeGet(key);
                return eq(dictEq)(get(ra))(get(rb)) && tail;
              };
            };
          });
        };
      };
    };
  };

  var notEq = function notEq(dictEq) {
    return function (x) {
      return function (y) {
        return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
      };
    };
  };

  exports["Eq"] = Eq;
  exports["eq"] = eq;
  exports["notEq"] = notEq;
  exports["eqBoolean"] = eqBoolean;
  exports["eqInt"] = eqInt;
  exports["eqChar"] = eqChar;
  exports["eqString"] = eqString;
  exports["eqUnit"] = eqUnit;
  exports["eqRec"] = eqRec;
  exports["eqRowNil"] = eqRowNil;
  exports["eqRowCons"] = eqRowCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];

  var Nothing = function () {
    function Nothing() {}

    ;
    Nothing.value = new Nothing();
    return Nothing;
  }();

  var Just = function () {
    function Just(value0) {
      this.value0 = value0;
    }

    ;

    Just.create = function (value0) {
      return new Just(value0);
    };

    return Just;
  }();

  var maybe = function maybe(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Nothing) {
          return v;
        }

        ;

        if (v2 instanceof Just) {
          return v1(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  var isNothing = maybe(true)(Data_Function["const"](false));
  var isJust = maybe(false)(Data_Function["const"](true));
  var functorMaybe = new Data_Functor.Functor(function (v) {
    return function (v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }

      ;
      return Nothing.value;
    };
  });

  var fromMaybe = function fromMaybe(a) {
    return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };

  var fromJust = function fromJust(dictPartial) {
    return function (v) {
      if (v instanceof Just) {
        return v.value0;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 268, column 1 - line 268, column 46): " + [v.constructor.name]);
    };
  };

  var eqMaybe = function eqMaybe(dictEq) {
    return new Data_Eq.Eq(function (x) {
      return function (y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return true;
        }

        ;

        if (x instanceof Just && y instanceof Just) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0);
        }

        ;
        return false;
      };
    });
  };

  var applyMaybe = new Control_Apply.Apply(function () {
    return functorMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Just) {
        return Data_Functor.map(functorMaybe)(v.value0)(v1);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var bindMaybe = new Control_Bind.Bind(function () {
    return applyMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var applicativeMaybe = new Control_Applicative.Applicative(function () {
    return applyMaybe;
  }, Just.create);
  var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Nothing) {
        return v1;
      }

      ;
      return v;
    };
  });
  var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
  }, Nothing.value);
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["maybe"] = maybe;
  exports["fromMaybe"] = fromMaybe;
  exports["isJust"] = isJust;
  exports["isNothing"] = isNothing;
  exports["fromJust"] = fromJust;
  exports["functorMaybe"] = functorMaybe;
  exports["applyMaybe"] = applyMaybe;
  exports["applicativeMaybe"] = applicativeMaybe;
  exports["plusMaybe"] = plusMaybe;
  exports["bindMaybe"] = bindMaybe;
  exports["eqMaybe"] = eqMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.MediaType.Common"] = $PS["Data.MediaType.Common"] || {};
  var exports = $PS["Data.MediaType.Common"];
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";
  exports["applicationFormURLEncoded"] = applicationFormURLEncoded;
  exports["applicationJSON"] = applicationJSON;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Affjax.RequestBody"] = $PS["Affjax.RequestBody"] || {};
  var exports = $PS["Affjax.RequestBody"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_MediaType_Common = $PS["Data.MediaType.Common"];

  var ArrayView = function () {
    function ArrayView(value0) {
      this.value0 = value0;
    }

    ;

    ArrayView.create = function (value0) {
      return new ArrayView(value0);
    };

    return ArrayView;
  }();

  var Blob = function () {
    function Blob(value0) {
      this.value0 = value0;
    }

    ;

    Blob.create = function (value0) {
      return new Blob(value0);
    };

    return Blob;
  }();

  var Document = function () {
    function Document(value0) {
      this.value0 = value0;
    }

    ;

    Document.create = function (value0) {
      return new Document(value0);
    };

    return Document;
  }();

  var $$String = function () {
    function $$String(value0) {
      this.value0 = value0;
    }

    ;

    $$String.create = function (value0) {
      return new $$String(value0);
    };

    return $$String;
  }();

  var FormData = function () {
    function FormData(value0) {
      this.value0 = value0;
    }

    ;

    FormData.create = function (value0) {
      return new FormData(value0);
    };

    return FormData;
  }();

  var FormURLEncoded = function () {
    function FormURLEncoded(value0) {
      this.value0 = value0;
    }

    ;

    FormURLEncoded.create = function (value0) {
      return new FormURLEncoded(value0);
    };

    return FormURLEncoded;
  }();

  var Json = function () {
    function Json(value0) {
      this.value0 = value0;
    }

    ;

    Json.create = function (value0) {
      return new Json(value0);
    };

    return Json;
  }();

  var toMediaType = function toMediaType(v) {
    if (v instanceof FormURLEncoded) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationFormURLEncoded);
    }

    ;

    if (v instanceof Json) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationJSON);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var json = Json.create;
  exports["ArrayView"] = ArrayView;
  exports["Blob"] = Blob;
  exports["Document"] = Document;
  exports["String"] = $$String;
  exports["FormData"] = FormData;
  exports["FormURLEncoded"] = FormURLEncoded;
  exports["Json"] = Json;
  exports["json"] = json;
  exports["toMediaType"] = toMediaType;
})(PS);

(function (exports) {
  "use strict";

  exports.boolConj = function (b1) {
    return function (b2) {
      return b1 && b2;
    };
  };

  exports.boolDisj = function (b1) {
    return function (b2) {
      return b1 || b2;
    };
  };

  exports.boolNot = function (b) {
    return !b;
  };
})(PS["Data.HeytingAlgebra"] = PS["Data.HeytingAlgebra"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.HeytingAlgebra"] = $PS["Data.HeytingAlgebra"] || {};
  var exports = $PS["Data.HeytingAlgebra"];
  var $foreign = $PS["Data.HeytingAlgebra"];

  var HeytingAlgebra = function HeytingAlgebra(conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
  };

  var tt = function tt(dict) {
    return dict.tt;
  };

  var not = function not(dict) {
    return dict.not;
  };

  var implies = function implies(dict) {
    return dict.implies;
  };

  var ff = function ff(dict) {
    return dict.ff;
  };

  var disj = function disj(dict) {
    return dict.disj;
  };

  var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  }, $foreign.boolNot, true);

  var conj = function conj(dict) {
    return dict.conj;
  };

  var heytingAlgebraFunction = function heytingAlgebraFunction(dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
      return function (g) {
        return function (a) {
          return conj(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (f) {
      return function (g) {
        return function (a) {
          return disj(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (v) {
      return ff(dictHeytingAlgebra);
    }, function (f) {
      return function (g) {
        return function (a) {
          return implies(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (f) {
      return function (a) {
        return not(dictHeytingAlgebra)(f(a));
      };
    }, function (v) {
      return tt(dictHeytingAlgebra);
    });
  };

  exports["ff"] = ff;
  exports["disj"] = disj;
  exports["not"] = not;
  exports["heytingAlgebraBoolean"] = heytingAlgebraBoolean;
  exports["heytingAlgebraFunction"] = heytingAlgebraFunction;
})(PS);

(function (exports) {
  "use strict";

  exports.concatString = function (s1) {
    return function (s2) {
      return s1 + s2;
    };
  };

  exports.concatArray = function (xs) {
    return function (ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };
})(PS["Data.Semigroup"] = PS["Data.Semigroup"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Semigroup"] = $PS["Data.Semigroup"] || {};
  var exports = $PS["Data.Semigroup"];
  var $foreign = $PS["Data.Semigroup"];

  var Semigroup = function Semigroup(append) {
    this.append = append;
  };

  var semigroupString = new Semigroup($foreign.concatString);
  var semigroupArray = new Semigroup($foreign.concatArray);

  var append = function append(dict) {
    return dict.append;
  };

  var semigroupFn = function semigroupFn(dictSemigroup) {
    return new Semigroup(function (f) {
      return function (g) {
        return function (x) {
          return append(dictSemigroup)(f(x))(g(x));
        };
      };
    });
  };

  exports["Semigroup"] = Semigroup;
  exports["append"] = append;
  exports["semigroupString"] = semigroupString;
  exports["semigroupFn"] = semigroupFn;
  exports["semigroupArray"] = semigroupArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Monoid"] = $PS["Data.Monoid"] || {};
  var exports = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Monoid = function Monoid(Semigroup0, mempty) {
    this.Semigroup0 = Semigroup0;
    this.mempty = mempty;
  };

  var monoidString = new Monoid(function () {
    return Data_Semigroup.semigroupString;
  }, "");

  var mempty = function mempty(dict) {
    return dict.mempty;
  };

  var monoidFn = function monoidFn(dictMonoid) {
    return new Monoid(function () {
      return Data_Semigroup.semigroupFn(dictMonoid.Semigroup0());
    }, function (v) {
      return mempty(dictMonoid);
    });
  };

  var guard = function guard(dictMonoid) {
    return function (v) {
      return function (v1) {
        if (v) {
          return v1;
        }

        ;

        if (!v) {
          return mempty(dictMonoid);
        }

        ;
        throw new Error("Failed pattern match at Data.Monoid (line 73, column 1 - line 73, column 49): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };

  exports["Monoid"] = Monoid;
  exports["mempty"] = mempty;
  exports["guard"] = guard;
  exports["monoidFn"] = monoidFn;
  exports["monoidString"] = monoidString;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Monoid.Disj"] = $PS["Data.Monoid.Disj"] || {};
  var exports = $PS["Data.Monoid.Disj"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Disj = function Disj(x) {
    return x;
  };

  var semigroupDisj = function semigroupDisj(dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
        return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
      };
    });
  };

  var monoidDisj = function monoidDisj(dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
      return semigroupDisj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
  };

  exports["Disj"] = Disj;
  exports["monoidDisj"] = monoidDisj;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Newtype"] = $PS["Data.Newtype"] || {};
  var exports = $PS["Data.Newtype"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];

  var Newtype = function Newtype(unwrap, wrap) {
    this.unwrap = unwrap;
    this.wrap = wrap;
  };

  var wrap = function wrap(dict) {
    return dict.wrap;
  };

  var unwrap = function unwrap(dict) {
    return dict.unwrap;
  };

  var under = function under(dictNewtype) {
    return function (dictNewtype1) {
      return function (v) {
        return function (f) {
          var $75 = unwrap(dictNewtype1);
          var $76 = wrap(dictNewtype);
          return function ($77) {
            return $75(f($76($77)));
          };
        };
      };
    };
  };

  var over = function over(dictNewtype) {
    return function (dictNewtype1) {
      return function (v) {
        return function (f) {
          var $90 = wrap(dictNewtype1);
          var $91 = unwrap(dictNewtype);
          return function ($92) {
            return $90(f($91($92)));
          };
        };
      };
    };
  };

  var newtypeDisj = new Newtype(function (v) {
    return v;
  }, Data_Monoid_Disj.Disj);

  var alaF = function alaF(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (v) {
            return function (f) {
              var $96 = Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1));
              var $97 = Data_Functor.map(dictFunctor)(wrap(dictNewtype));
              return function ($98) {
                return $96(f($97($98)));
              };
            };
          };
        };
      };
    };
  };

  exports["unwrap"] = unwrap;
  exports["wrap"] = wrap;
  exports["Newtype"] = Newtype;
  exports["alaF"] = alaF;
  exports["over"] = over;
  exports["under"] = under;
  exports["newtypeDisj"] = newtypeDisj;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.MediaType"] = $PS["Data.MediaType"] || {};
  var exports = $PS["Data.MediaType"];
  var Data_Newtype = $PS["Data.Newtype"];

  var MediaType = function MediaType(x) {
    return x;
  };

  var newtypeMediaType = new Data_Newtype.Newtype(function (n) {
    return n;
  }, MediaType);
  exports["newtypeMediaType"] = newtypeMediaType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Affjax.RequestHeader"] = $PS["Affjax.RequestHeader"] || {};
  var exports = $PS["Affjax.RequestHeader"];
  var Data_MediaType = $PS["Data.MediaType"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Accept = function () {
    function Accept(value0) {
      this.value0 = value0;
    }

    ;

    Accept.create = function (value0) {
      return new Accept(value0);
    };

    return Accept;
  }();

  var ContentType = function () {
    function ContentType(value0) {
      this.value0 = value0;
    }

    ;

    ContentType.create = function (value0) {
      return new ContentType(value0);
    };

    return ContentType;
  }();

  var RequestHeader = function () {
    function RequestHeader(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    RequestHeader.create = function (value0) {
      return function (value1) {
        return new RequestHeader(value0, value1);
      };
    };

    return RequestHeader;
  }();

  var value = function value(v) {
    if (v instanceof Accept) {
      return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    }

    ;

    if (v instanceof ContentType) {
      return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    }

    ;

    if (v instanceof RequestHeader) {
      return v.value1;
    }

    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };

  var name = function name(v) {
    if (v instanceof Accept) {
      return "Accept";
    }

    ;

    if (v instanceof ContentType) {
      return "Content-Type";
    }

    ;

    if (v instanceof RequestHeader) {
      return v.value0;
    }

    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  exports["Accept"] = Accept;
  exports["ContentType"] = ContentType;
  exports["RequestHeader"] = RequestHeader;
  exports["name"] = name;
  exports["value"] = value;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeToForeign = function (value) {
    return value;
  };

  exports.unsafeFromForeign = function (value) {
    return value;
  };

  exports.typeOf = function (value) {
    return _typeof(value);
  };

  exports.tagOf = function (value) {
    return Object.prototype.toString.call(value).slice(8, -1);
  };
})(PS["Foreign"] = PS["Foreign"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Bifunctor"] = $PS["Data.Bifunctor"] || {};
  var exports = $PS["Data.Bifunctor"];
  var Control_Category = $PS["Control.Category"];

  var Bifunctor = function Bifunctor(bimap) {
    this.bimap = bimap;
  };

  var bimap = function bimap(dict) {
    return dict.bimap;
  };

  var lmap = function lmap(dictBifunctor) {
    return function (f) {
      return bimap(dictBifunctor)(f)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var rmap = function rmap(dictBifunctor) {
    return bimap(dictBifunctor)(Control_Category.identity(Control_Category.categoryFn));
  };

  exports["bimap"] = bimap;
  exports["Bifunctor"] = Bifunctor;
  exports["lmap"] = lmap;
  exports["rmap"] = rmap;
})(PS);

(function (exports) {
  "use strict";

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;

        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }

        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;

        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }

        return acc;
      };
    };
  };
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Foldable"] = $PS["Data.Foldable"] || {};
  var exports = $PS["Data.Foldable"];
  var $foreign = $PS["Data.Foldable"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Unit = $PS["Data.Unit"];

  var Foldable = function Foldable(foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
  };

  var foldr = function foldr(dict) {
    return dict.foldr;
  };

  var traverse_ = function traverse_(dictApplicative) {
    return function (dictFoldable) {
      return function (f) {
        return foldr(dictFoldable)(function () {
          var $197 = Control_Apply.applySecond(dictApplicative.Apply0());
          return function ($198) {
            return $197(f($198));
          };
        }())(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
      };
    };
  };

  var for_ = function for_(dictApplicative) {
    return function (dictFoldable) {
      return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
  };

  var sequence_ = function sequence_(dictApplicative) {
    return function (dictFoldable) {
      return traverse_(dictApplicative)(dictFoldable)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var foldl = function foldl(dict) {
    return dict.foldl;
  };

  var indexl = function indexl(dictFoldable) {
    return function (idx) {
      var go = function go(cursor) {
        return function (a) {
          if (cursor.elem instanceof Data_Maybe.Just) {
            return cursor;
          }

          ;
          var $109 = cursor.pos === idx;

          if ($109) {
            return {
              elem: new Data_Maybe.Just(a),
              pos: cursor.pos
            };
          }

          ;
          return {
            pos: cursor.pos + 1 | 0,
            elem: cursor.elem
          };
        };
      };

      var $199 = foldl(dictFoldable)(go)({
        elem: Data_Maybe.Nothing.value,
        pos: 0
      });
      return function ($200) {
        return function (v) {
          return v.elem;
        }($199($200));
      };
    };
  };

  var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
      return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
          return Data_Monoid.mempty(dictMonoid);
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return f(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [f.constructor.name, v.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return z;
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return v(z)(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return z;
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return v(v1.value0)(z);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  });

  var foldMapDefaultR = function foldMapDefaultR(dictFoldable) {
    return function (dictMonoid) {
      return function (f) {
        return foldr(dictFoldable)(function (x) {
          return function (acc) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(Data_Monoid.mempty(dictMonoid));
      };
    };
  };

  var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);

  var foldMap = function foldMap(dict) {
    return dict.foldMap;
  };

  var any = function any(dictFoldable) {
    return function (dictHeytingAlgebra) {
      return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Newtype.newtypeDisj)(Data_Newtype.newtypeDisj)(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
    };
  };

  var elem = function elem(dictFoldable) {
    return function (dictEq) {
      var $204 = any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean);
      var $205 = Data_Eq.eq(dictEq);
      return function ($206) {
        return $204($205($206));
      };
    };
  };

  var notElem = function notElem(dictFoldable) {
    return function (dictEq) {
      return function (x) {
        var $207 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
        var $208 = elem(dictFoldable)(dictEq)(x);
        return function ($209) {
          return $207($208($209));
        };
      };
    };
  };

  exports["Foldable"] = Foldable;
  exports["foldr"] = foldr;
  exports["foldl"] = foldl;
  exports["foldMap"] = foldMap;
  exports["traverse_"] = traverse_;
  exports["for_"] = for_;
  exports["sequence_"] = sequence_;
  exports["any"] = any;
  exports["elem"] = elem;
  exports["notElem"] = notElem;
  exports["indexl"] = indexl;
  exports["foldableArray"] = foldableArray;
  exports["foldableMaybe"] = foldableMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Either"] = $PS["Data.Either"] || {};
  var exports = $PS["Data.Either"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];

  var Left = function () {
    function Left(value0) {
      this.value0 = value0;
    }

    ;

    Left.create = function (value0) {
      return new Left(value0);
    };

    return Left;
  }();

  var Right = function () {
    function Right(value0) {
      this.value0 = value0;
    }

    ;

    Right.create = function (value0) {
      return new Right(value0);
    };

    return Right;
  }();

  var note = function note(a) {
    return Data_Maybe.maybe(new Left(a))(Right.create);
  };

  var functorEither = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }

      ;

      if (m instanceof Right) {
        return new Right(f(m.value0));
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 38, column 1 - line 38, column 52): " + [m.constructor.name]);
    };
  });
  var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (v) {
        if (v instanceof Left) {
          return Data_Monoid.mempty(dictMonoid);
        }

        ;

        if (v instanceof Right) {
          return f(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 193, column 28): " + [f.constructor.name, v.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Left) {
          return z;
        }

        ;

        if (v1 instanceof Right) {
          return v(z)(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 193, column 28): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Left) {
          return z;
        }

        ;

        if (v1 instanceof Right) {
          return v(v1.value0)(z);
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 193, column 28): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  });

  var either = function either(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }

        ;

        if (v2 instanceof Right) {
          return v1(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  var hush = either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
  var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Left) {
          return new Left(v(v2.value0));
        }

        ;

        if (v2 instanceof Right) {
          return new Right(v1(v2.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 46, column 1 - line 48, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  });
  var applyEither = new Control_Apply.Apply(function () {
    return functorEither;
  }, function (v) {
    return function (v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }

      ;

      if (v instanceof Right) {
        return Data_Functor.map(functorEither)(v.value0)(v1);
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 82, column 1 - line 84, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var bindEither = new Control_Bind.Bind(function () {
    return applyEither;
  }, either(function (e) {
    return function (v) {
      return new Left(e);
    };
  })(function (a) {
    return function (f) {
      return f(a);
    };
  }));
  var applicativeEither = new Control_Applicative.Applicative(function () {
    return applyEither;
  }, Right.create);
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["note"] = note;
  exports["hush"] = hush;
  exports["functorEither"] = functorEither;
  exports["bifunctorEither"] = bifunctorEither;
  exports["applyEither"] = applyEither;
  exports["applicativeEither"] = applicativeEither;
  exports["bindEither"] = bindEither;
  exports["foldableEither"] = foldableEither;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Error.Class"] = $PS["Control.Monad.Error.Class"] || {};
  var exports = $PS["Control.Monad.Error.Class"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];

  var MonadThrow = function MonadThrow(Monad0, throwError) {
    this.Monad0 = Monad0;
    this.throwError = throwError;
  };

  var MonadError = function MonadError(MonadThrow0, catchError) {
    this.MonadThrow0 = MonadThrow0;
    this.catchError = catchError;
  };

  var throwError = function throwError(dict) {
    return dict.throwError;
  };

  var catchError = function catchError(dict) {
    return dict.catchError;
  };

  var $$try = function $$try(dictMonadError) {
    return function (a) {
      return catchError(dictMonadError)(Data_Functor.map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Data_Either.Right.create)(a))(function () {
        var $21 = Control_Applicative.pure(dictMonadError.MonadThrow0().Monad0().Applicative0());
        return function ($22) {
          return $21(Data_Either.Left.create($22));
        };
      }());
    };
  };

  exports["throwError"] = throwError;
  exports["MonadThrow"] = MonadThrow;
  exports["MonadError"] = MonadError;
  exports["try"] = $$try;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];

  var Monad = function Monad(Applicative0, Bind1) {
    this.Applicative0 = Applicative0;
    this.Bind1 = Bind1;
  };

  var unlessM = function unlessM(dictMonad) {
    return function (mb) {
      return function (m) {
        return Control_Bind.bind(dictMonad.Bind1())(mb)(function (v) {
          return Control_Applicative.unless(dictMonad.Applicative0())(v)(m);
        });
      };
    };
  };

  var ap = function ap(dictMonad) {
    return function (f) {
      return function (a) {
        return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
          return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
            return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
          });
        });
      };
    };
  };

  exports["Monad"] = Monad;
  exports["ap"] = ap;
  exports["unlessM"] = unlessM;
})(PS);

(function (exports) {
  "use strict";

  var unsafeCompareImpl = function unsafeCompareImpl(lt) {
    return function (eq) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return x < y ? lt : x === y ? eq : gt;
          };
        };
      };
    };
  };

  exports.ordIntImpl = unsafeCompareImpl;
  exports.ordStringImpl = unsafeCompareImpl;
  exports.ordCharImpl = unsafeCompareImpl;
})(PS["Data.Ord"] = PS["Data.Ord"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Ordering"] = $PS["Data.Ordering"] || {};
  var exports = $PS["Data.Ordering"];

  var LT = function () {
    function LT() {}

    ;
    LT.value = new LT();
    return LT;
  }();

  var GT = function () {
    function GT() {}

    ;
    GT.value = new GT();
    return GT;
  }();

  var EQ = function () {
    function EQ() {}

    ;
    EQ.value = new EQ();
    return EQ;
  }();

  exports["LT"] = LT;
  exports["GT"] = GT;
  exports["EQ"] = EQ;
})(PS);

(function (exports) {
  "use strict";

  exports.intSub = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x - y | 0;
    };
  };
})(PS["Data.Ring"] = PS["Data.Ring"] || {});

(function (exports) {
  "use strict";

  exports.intAdd = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x + y | 0;
    };
  };

  exports.intMul = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x * y | 0;
    };
  };
})(PS["Data.Semiring"] = PS["Data.Semiring"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Semiring"] = $PS["Data.Semiring"] || {};
  var exports = $PS["Data.Semiring"];
  var $foreign = $PS["Data.Semiring"];

  var Semiring = function Semiring(add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
  };

  var zero = function zero(dict) {
    return dict.zero;
  };

  var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
  exports["zero"] = zero;
  exports["semiringInt"] = semiringInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Ring"] = $PS["Data.Ring"] || {};
  var exports = $PS["Data.Ring"];
  var $foreign = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];

  var Ring = function Ring(Semiring0, sub) {
    this.Semiring0 = Semiring0;
    this.sub = sub;
  };

  var sub = function sub(dict) {
    return dict.sub;
  };

  var ringInt = new Ring(function () {
    return Data_Semiring.semiringInt;
  }, $foreign.intSub);

  var negate = function negate(dictRing) {
    return function (a) {
      return sub(dictRing)(Data_Semiring.zero(dictRing.Semiring0()))(a);
    };
  };

  exports["negate"] = negate;
  exports["ringInt"] = ringInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Ord"] = $PS["Data.Ord"] || {};
  var exports = $PS["Data.Ord"];
  var $foreign = $PS["Data.Ord"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];

  var Ord = function Ord(Eq0, compare) {
    this.Eq0 = Eq0;
    this.compare = compare;
  };

  var ordUnit = new Ord(function () {
    return Data_Eq.eqUnit;
  }, function (v) {
    return function (v1) {
      return Data_Ordering.EQ.value;
    };
  });
  var ordString = new Ord(function () {
    return Data_Eq.eqString;
  }, $foreign.ordStringImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordInt = new Ord(function () {
    return Data_Eq.eqInt;
  }, $foreign.ordIntImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordChar = new Ord(function () {
    return Data_Eq.eqChar;
  }, $foreign.ordCharImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));

  var compare = function compare(dict) {
    return dict.compare;
  };

  var greaterThanOrEq = function greaterThanOrEq(dictOrd) {
    return function (a1) {
      return function (a2) {
        var v = compare(dictOrd)(a1)(a2);

        if (v instanceof Data_Ordering.LT) {
          return false;
        }

        ;
        return true;
      };
    };
  };

  var abs = function abs(dictOrd) {
    return function (dictRing) {
      return function (x) {
        var $53 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));

        if ($53) {
          return x;
        }

        ;
        return Data_Ring.negate(dictRing)(x);
      };
    };
  };

  exports["Ord"] = Ord;
  exports["compare"] = compare;
  exports["abs"] = abs;
  exports["ordInt"] = ordInt;
  exports["ordString"] = ordString;
  exports["ordChar"] = ordChar;
  exports["ordUnit"] = ordUnit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Tuple"] = $PS["Data.Tuple"] || {};
  var exports = $PS["Data.Tuple"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];

  var Tuple = function () {
    function Tuple(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Tuple.create = function (value0) {
      return function (value1) {
        return new Tuple(value0, value1);
      };
    };

    return Tuple;
  }();

  var uncurry = function uncurry(f) {
    return function (v) {
      return f(v.value0)(v.value1);
    };
  };

  var snd = function snd(v) {
    return v.value1;
  };

  var functorTuple = new Data_Functor.Functor(function (f) {
    return function (m) {
      return new Tuple(m.value0, f(m.value1));
    };
  });

  var fst = function fst(v) {
    return v.value0;
  };

  var eqTuple = function eqTuple(dictEq) {
    return function (dictEq1) {
      return new Data_Eq.Eq(function (x) {
        return function (y) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
        };
      });
    };
  };

  var ordTuple = function ordTuple(dictOrd) {
    return function (dictOrd1) {
      return new Data_Ord.Ord(function () {
        return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
      }, function (x) {
        return function (y) {
          var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);

          if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
          }

          ;

          if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
          }

          ;
          return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
        };
      });
    };
  };

  var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        return new Tuple(f(v.value0), g(v.value1));
      };
    };
  });
  exports["Tuple"] = Tuple;
  exports["fst"] = fst;
  exports["snd"] = snd;
  exports["uncurry"] = uncurry;
  exports["ordTuple"] = ordTuple;
  exports["functorTuple"] = functorTuple;
  exports["bifunctorTuple"] = bifunctorTuple;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.State.Class"] = $PS["Control.Monad.State.Class"] || {};
  var exports = $PS["Control.Monad.State.Class"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];

  var MonadState = function MonadState(Monad0, state) {
    this.Monad0 = Monad0;
    this.state = state;
  };

  var state = function state(dict) {
    return dict.state;
  };

  var put = function put(dictMonadState) {
    return function (s) {
      return state(dictMonadState)(function (v) {
        return new Data_Tuple.Tuple(Data_Unit.unit, s);
      });
    };
  };

  var modify_ = function modify_(dictMonadState) {
    return function (f) {
      return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
      });
    };
  };

  var modify = function modify(dictMonadState) {
    return function (f) {
      return state(dictMonadState)(function (s) {
        var s$prime = f(s);
        return new Data_Tuple.Tuple(s$prime, s$prime);
      });
    };
  };

  var gets = function gets(dictMonadState) {
    return function (f) {
      return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(f(s), s);
      });
    };
  };

  var get = function get(dictMonadState) {
    return state(dictMonadState)(function (s) {
      return new Data_Tuple.Tuple(s, s);
    });
  };

  exports["state"] = state;
  exports["MonadState"] = MonadState;
  exports["get"] = get;
  exports["gets"] = gets;
  exports["put"] = put;
  exports["modify"] = modify;
  exports["modify_"] = modify_;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Trans.Class"] = $PS["Control.Monad.Trans.Class"] || {};
  var exports = $PS["Control.Monad.Trans.Class"];

  var MonadTrans = function MonadTrans(lift) {
    this.lift = lift;
  };

  var lift = function lift(dict) {
    return dict.lift;
  };

  exports["lift"] = lift;
  exports["MonadTrans"] = MonadTrans;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Except.Trans"] = $PS["Control.Monad.Except.Trans"] || {};
  var exports = $PS["Control.Monad.Except.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];

  var ExceptT = function ExceptT(x) {
    return x;
  };

  var runExceptT = function runExceptT(v) {
    return v;
  };

  var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
      return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v));
      });
    };
  });

  var mapExceptT = function mapExceptT(f) {
    return function (v) {
      return f(v);
    };
  };

  var functorExceptT = function functorExceptT(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
    });
  };

  var monadExceptT = function monadExceptT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeExceptT(dictMonad);
    }, function () {
      return bindExceptT(dictMonad);
    });
  };

  var bindExceptT = function bindExceptT(dictMonad) {
    return new Control_Bind.Bind(function () {
      return applyExceptT(dictMonad);
    }, function (v) {
      return function (k) {
        return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function () {
          var $98 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($99) {
            return $98(Data_Either.Left.create($99));
          };
        }())(function (a) {
          var v1 = k(a);
          return v1;
        }));
      };
    });
  };

  var applyExceptT = function applyExceptT(dictMonad) {
    return new Control_Apply.Apply(function () {
      return functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    }, Control_Monad.ap(monadExceptT(dictMonad)));
  };

  var applicativeExceptT = function applicativeExceptT(dictMonad) {
    return new Control_Applicative.Applicative(function () {
      return applyExceptT(dictMonad);
    }, function () {
      var $100 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($101) {
        return ExceptT($100(Data_Either.Right.create($101)));
      };
    }());
  };

  var monadStateExceptT = function monadStateExceptT(dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
      return monadExceptT(dictMonadState.Monad0());
    }, function (f) {
      return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
  };

  var monadThrowExceptT = function monadThrowExceptT(dictMonad) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
      return monadExceptT(dictMonad);
    }, function () {
      var $110 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($111) {
        return ExceptT($110(Data_Either.Left.create($111)));
      };
    }());
  };

  exports["ExceptT"] = ExceptT;
  exports["runExceptT"] = runExceptT;
  exports["functorExceptT"] = functorExceptT;
  exports["applyExceptT"] = applyExceptT;
  exports["applicativeExceptT"] = applicativeExceptT;
  exports["bindExceptT"] = bindExceptT;
  exports["monadThrowExceptT"] = monadThrowExceptT;
  exports["monadStateExceptT"] = monadStateExceptT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Boolean"] = $PS["Data.Boolean"] || {};
  var exports = $PS["Data.Boolean"];
  var otherwise = true;
  exports["otherwise"] = otherwise;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Identity"] = $PS["Data.Identity"] || {};
  var exports = $PS["Data.Identity"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Identity = function Identity(x) {
    return x;
  };

  var newtypeIdentity = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Identity);
  var functorIdentity = new Data_Functor.Functor(function (f) {
    return function (m) {
      return f(m);
    };
  });
  var applyIdentity = new Control_Apply.Apply(function () {
    return functorIdentity;
  }, function (v) {
    return function (v1) {
      return v(v1);
    };
  });
  var bindIdentity = new Control_Bind.Bind(function () {
    return applyIdentity;
  }, function (v) {
    return function (f) {
      return f(v);
    };
  });
  var applicativeIdentity = new Control_Applicative.Applicative(function () {
    return applyIdentity;
  }, Identity);
  var monadIdentity = new Control_Monad.Monad(function () {
    return applicativeIdentity;
  }, function () {
    return bindIdentity;
  });
  exports["newtypeIdentity"] = newtypeIdentity;
  exports["functorIdentity"] = functorIdentity;
  exports["applicativeIdentity"] = applicativeIdentity;
  exports["monadIdentity"] = monadIdentity;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.List.Types"] = $PS["Data.List.Types"] || {};
  var exports = $PS["Data.List.Types"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Nil = function () {
    function Nil() {}

    ;
    Nil.value = new Nil();
    return Nil;
  }();

  var Cons = function () {
    function Cons(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Cons.create = function (value0) {
      return function (value1) {
        return new Cons(value0, value1);
      };
    };

    return Cons;
  }();

  var NonEmptyList = function NonEmptyList(x) {
    return x;
  };

  var listMap = function listMap(f) {
    var chunkedRevMap = function chunkedRevMap($copy_chunksAcc) {
      return function ($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && v.value1 instanceof Cons && v.value1.value1 instanceof Cons) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }

          ;

          var unrolledMap = function unrolledMap(v1) {
            if (v1 instanceof Cons && v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }

            ;

            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }

            ;
            return Nil.value;
          };

          var reverseUnrolledMap = function reverseUnrolledMap($copy_v1) {
            return function ($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(v1, acc) {
                if (v1 instanceof Cons && v1.value0 instanceof Cons && v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }

                ;
                $tco_done = true;
                return acc;
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v1, $copy_acc);
              }

              ;
              return $tco_result;
            };
          };

          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return chunkedRevMap(Nil.value);
  };

  var functorList = new Data_Functor.Functor(listMap);
  var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return Data_Foldable.foldl(foldableList)(function (acc) {
        var $202 = Data_Semigroup.append(dictMonoid.Semigroup0())(acc);
        return function ($203) {
          return $202(f($203));
        };
      })(Data_Monoid.mempty(dictMonoid));
    };
  }, function (f) {
    var go = function go($copy_b) {
      return function ($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return b;
          }

          ;

          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.List.Types (line 109, column 12 - line 111, column 30): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go;
  }, function (f) {
    return function (b) {
      var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
      var $204 = Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b);
      return function ($205) {
        return $204(rev($205));
      };
    };
  });
  var semigroupList = new Data_Semigroup.Semigroup(function (xs) {
    return function (ys) {
      return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
    };
  });
  var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
  }, Nil.value);
  var applyList = new Control_Apply.Apply(function () {
    return functorList;
  }, function (v) {
    return function (v1) {
      if (v instanceof Nil) {
        return Nil.value;
      }

      ;

      if (v instanceof Cons) {
        return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
      }

      ;
      throw new Error("Failed pattern match at Data.List.Types (line 155, column 1 - line 157, column 48): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var applicativeList = new Control_Applicative.Applicative(function () {
    return applyList;
  }, function (a) {
    return new Cons(a, Nil.value);
  });
  var altList = new Control_Alt.Alt(function () {
    return functorList;
  }, Data_Semigroup.append(semigroupList));
  var plusList = new Control_Plus.Plus(function () {
    return altList;
  }, Nil.value);
  exports["Nil"] = Nil;
  exports["Cons"] = Cons;
  exports["NonEmptyList"] = NonEmptyList;
  exports["semigroupList"] = semigroupList;
  exports["monoidList"] = monoidList;
  exports["foldableList"] = foldableList;
  exports["applicativeList"] = applicativeList;
  exports["plusList"] = plusList;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.NonEmpty"] = $PS["Data.NonEmpty"] || {};
  var exports = $PS["Data.NonEmpty"];
  var Control_Plus = $PS["Control.Plus"];

  var NonEmpty = function () {
    function NonEmpty(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    NonEmpty.create = function (value0) {
      return function (value1) {
        return new NonEmpty(value0, value1);
      };
    };

    return NonEmpty;
  }();

  var singleton = function singleton(dictPlus) {
    return function (a) {
      return new NonEmpty(a, Control_Plus.empty(dictPlus));
    };
  };

  exports["NonEmpty"] = NonEmpty;
  exports["singleton"] = singleton;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.List.NonEmpty"] = $PS["Data.List.NonEmpty"] || {};
  var exports = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];

  var singleton = function () {
    var $168 = Data_NonEmpty.singleton(Data_List_Types.plusList);
    return function ($169) {
      return Data_List_Types.NonEmptyList($168($169));
    };
  }();

  var head = function head(v) {
    return v.value0;
  };

  var cons = function cons(y) {
    return function (v) {
      return new Data_NonEmpty.NonEmpty(y, new Data_List_Types.Cons(v.value0, v.value1));
    };
  };

  exports["singleton"] = singleton;
  exports["cons"] = cons;
  exports["head"] = head;
})(PS);

(function (exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };

  exports.showCharImpl = function (c) {
    var code = c.charCodeAt(0);

    if (code < 0x20 || code === 0x7F) {
      switch (c) {
        case "\x07":
          return "'\\a'";

        case "\b":
          return "'\\b'";

        case "\f":
          return "'\\f'";

        case "\n":
          return "'\\n'";

        case "\r":
          return "'\\r'";

        case "\t":
          return "'\\t'";

        case "\v":
          return "'\\v'";
      }

      return "'\\" + code.toString(10) + "'";
    }

    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(/[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
    function (c, i) {
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;

        case "\x07":
          return "\\a";

        case "\b":
          return "\\b";

        case "\f":
          return "\\f";

        case "\n":
          return "\\n";

        case "\r":
          return "\\r";

        case "\t":
          return "\\t";

        case "\v":
          return "\\v";
      }

      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }) + "\"";
  };

  exports.showArrayImpl = function (f) {
    return function (xs) {
      var ss = [];

      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }

      return "[" + ss.join(",") + "]";
    };
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];

  var Show = function Show(show) {
    this.show = show;
  };

  var showString = new Show($foreign.showStringImpl);
  var showInt = new Show($foreign.showIntImpl);
  var showChar = new Show($foreign.showCharImpl);

  var show = function show(dict) {
    return dict.show;
  };

  var showArray = function showArray(dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
  };

  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
  exports["showChar"] = showChar;
  exports["showString"] = showString;
  exports["showArray"] = showArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Foreign"] = $PS["Foreign"] || {};
  var exports = $PS["Foreign"];
  var $foreign = $PS["Foreign"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_Show = $PS["Data.Show"];

  var ForeignError = function () {
    function ForeignError(value0) {
      this.value0 = value0;
    }

    ;

    ForeignError.create = function (value0) {
      return new ForeignError(value0);
    };

    return ForeignError;
  }();

  var TypeMismatch = function () {
    function TypeMismatch(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    TypeMismatch.create = function (value0) {
      return function (value1) {
        return new TypeMismatch(value0, value1);
      };
    };

    return TypeMismatch;
  }();

  var ErrorAtIndex = function () {
    function ErrorAtIndex(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ErrorAtIndex.create = function (value0) {
      return function (value1) {
        return new ErrorAtIndex(value0, value1);
      };
    };

    return ErrorAtIndex;
  }();

  var ErrorAtProperty = function () {
    function ErrorAtProperty(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ErrorAtProperty.create = function (value0) {
      return function (value1) {
        return new ErrorAtProperty(value0, value1);
      };
    };

    return ErrorAtProperty;
  }();

  var renderForeignError = function renderForeignError(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }

    ;

    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + renderForeignError(v.value1)));
    }

    ;

    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + renderForeignError(v.value1)));
    }

    ;

    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }

    ;
    throw new Error("Failed pattern match at Foreign (line 72, column 1 - line 72, column 45): " + [v.constructor.name]);
  };

  var fail = function () {
    var $107 = Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Data_Identity.monadIdentity));
    return function ($108) {
      return $107(Data_List_NonEmpty.singleton($108));
    };
  }();

  var unsafeReadTagged = function unsafeReadTagged(tag) {
    return function (value) {
      if ($foreign.tagOf(value) === tag) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
      }

      ;

      if (Data_Boolean.otherwise) {
        return fail(new TypeMismatch(tag, $foreign.tagOf(value)));
      }

      ;
      throw new Error("Failed pattern match at Foreign (line 106, column 1 - line 106, column 55): " + [tag.constructor.name, value.constructor.name]);
    };
  };

  var readString = unsafeReadTagged("String");
  exports["ForeignError"] = ForeignError;
  exports["TypeMismatch"] = TypeMismatch;
  exports["renderForeignError"] = renderForeignError;
  exports["unsafeReadTagged"] = unsafeReadTagged;
  exports["readString"] = readString;
  exports["fail"] = fail;
  exports["unsafeToForeign"] = $foreign.unsafeToForeign;
  exports["typeOf"] = $foreign.typeOf;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Affjax.ResponseFormat"] = $PS["Affjax.ResponseFormat"] || {};
  var exports = $PS["Affjax.ResponseFormat"];
  var Control_Category = $PS["Control.Category"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_MediaType_Common = $PS["Data.MediaType.Common"];
  var Foreign = $PS["Foreign"];

  var ResponseFormatError = function () {
    function ResponseFormatError(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ResponseFormatError.create = function (value0) {
      return function (value1) {
        return new ResponseFormatError(value0, value1);
      };
    };

    return ResponseFormatError;
  }();

  var $$ArrayBuffer = function () {
    function $$ArrayBuffer(value0) {
      this.value0 = value0;
    }

    ;

    $$ArrayBuffer.create = function (value0) {
      return new $$ArrayBuffer(value0);
    };

    return $$ArrayBuffer;
  }();

  var Blob = function () {
    function Blob(value0) {
      this.value0 = value0;
    }

    ;

    Blob.create = function (value0) {
      return new Blob(value0);
    };

    return Blob;
  }();

  var Document = function () {
    function Document(value0) {
      this.value0 = value0;
    }

    ;

    Document.create = function (value0) {
      return new Document(value0);
    };

    return Document;
  }();

  var Json = function () {
    function Json(value0) {
      this.value0 = value0;
    }

    ;

    Json.create = function (value0) {
      return new Json(value0);
    };

    return Json;
  }();

  var $$String = function () {
    function $$String(value0) {
      this.value0 = value0;
    }

    ;

    $$String.create = function (value0) {
      return new $$String(value0);
    };

    return $$String;
  }();

  var Ignore = function () {
    function Ignore(value0) {
      this.value0 = value0;
    }

    ;

    Ignore.create = function (value0) {
      return new Ignore(value0);
    };

    return Ignore;
  }();

  var toResponseType = function toResponseType(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }

    ;

    if (v instanceof Blob) {
      return "blob";
    }

    ;

    if (v instanceof Document) {
      return "document";
    }

    ;

    if (v instanceof Json) {
      return "text";
    }

    ;

    if (v instanceof $$String) {
      return "text";
    }

    ;

    if (v instanceof Ignore) {
      return "";
    }

    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 46, column 3 - line 52, column 19): " + [v.constructor.name]);
  };

  var toMediaType = function toMediaType(v) {
    if (v instanceof Json) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationJSON);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var printResponseFormatError = function printResponseFormatError(v) {
    return Foreign.renderForeignError(v.value0);
  };

  var json = new Json(Control_Category.identity(Control_Category.categoryFn));
  exports["ArrayBuffer"] = $$ArrayBuffer;
  exports["Blob"] = Blob;
  exports["Document"] = Document;
  exports["Json"] = Json;
  exports["String"] = $$String;
  exports["Ignore"] = Ignore;
  exports["json"] = json;
  exports["toResponseType"] = toResponseType;
  exports["toMediaType"] = toMediaType;
  exports["ResponseFormatError"] = ResponseFormatError;
  exports["printResponseFormatError"] = printResponseFormatError;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Affjax.ResponseHeader"] = $PS["Affjax.ResponseHeader"] || {};
  var exports = $PS["Affjax.ResponseHeader"];

  var ResponseHeader = function () {
    function ResponseHeader(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ResponseHeader.create = function (value0) {
      return function (value1) {
        return new ResponseHeader(value0, value1);
      };
    };

    return ResponseHeader;
  }();

  exports["ResponseHeader"] = ResponseHeader;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Except"] = $PS["Control.Monad.Except"] || {};
  var exports = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Newtype = $PS["Data.Newtype"];

  var runExcept = function () {
    var $0 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
    return function ($1) {
      return $0(Control_Monad_Except_Trans.runExceptT($1));
    };
  }();

  exports["runExcept"] = runExcept;
})(PS);

(function (exports) {
  "use strict";

  function id(x) {
    return x;
  }

  exports.fromString = id;
  exports.fromObject = id;
  exports.jsonNull = null;

  exports.stringify = function (j) {
    return JSON.stringify(j);
  };

  var objToString = Object.prototype.toString;

  function isArray(a) {
    return objToString.call(a) === "[object Array]";
  }

  exports._caseJson = function (isNull, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null) return isNull();else if (typeof j === "boolean") return isBool(j);else if (typeof j === "number") return isNum(j);else if (typeof j === "string") return isStr(j);else if (objToString.call(j) === "[object Array]") return isArr(j);else return isObj(j);
  };
})(PS["Data.Argonaut.Core"] = PS["Data.Argonaut.Core"] || {});

(function (exports) {
  "use strict";

  exports._copyST = function (m) {
    return function () {
      var r = {};

      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }

      return r;
    };
  };

  exports.empty = {};

  exports.runST = function (f) {
    return f();
  };

  exports._fmapObject = function (m0, f) {
    var m = {};

    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(m0[k]);
      }
    }

    return m;
  };

  exports._mapWithKey = function (m0, f) {
    var m = {};

    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(k)(m0[k]);
      }
    }

    return m;
  };

  exports._foldM = function (bind) {
    return function (f) {
      return function (mz) {
        return function (m) {
          var acc = mz;

          function g(k) {
            return function (z) {
              return f(z)(k)(m[k]);
            };
          }

          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind(acc)(g(k));
            }
          }

          return acc;
        };
      };
    };
  };

  exports._lookup = function (no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  };

  function toArrayWithKey(f) {
    return function (m) {
      var r = [];

      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }

      return r;
    };
  }

  exports.toArrayWithKey = toArrayWithKey;
})(PS["Foreign.Object"] = PS["Foreign.Object"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.FoldableWithIndex"] = $PS["Data.FoldableWithIndex"] || {};
  var exports = $PS["Data.FoldableWithIndex"];

  var FoldableWithIndex = function FoldableWithIndex(Foldable0, foldMapWithIndex, foldlWithIndex, foldrWithIndex) {
    this.Foldable0 = Foldable0;
    this.foldMapWithIndex = foldMapWithIndex;
    this.foldlWithIndex = foldlWithIndex;
    this.foldrWithIndex = foldrWithIndex;
  };

  exports["FoldableWithIndex"] = FoldableWithIndex;
})(PS);

(function (exports) {
  "use strict";

  exports.runFn4 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Function.Uncurried"] = $PS["Data.Function.Uncurried"] || {};
  var exports = $PS["Data.Function.Uncurried"];
  var $foreign = $PS["Data.Function.Uncurried"];
  exports["runFn4"] = $foreign.runFn4;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.FunctorWithIndex"] = $PS["Data.FunctorWithIndex"] || {};
  var exports = $PS["Data.FunctorWithIndex"];

  var FunctorWithIndex = function FunctorWithIndex(Functor0, mapWithIndex) {
    this.Functor0 = Functor0;
    this.mapWithIndex = mapWithIndex;
  };

  exports["FunctorWithIndex"] = FunctorWithIndex;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Traversable"] = $PS["Data.Traversable"] || {};
  var exports = $PS["Data.Traversable"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Traversable = function Traversable(Foldable1, Functor0, sequence, traverse) {
    this.Foldable1 = Foldable1;
    this.Functor0 = Functor0;
    this.sequence = sequence;
    this.traverse = traverse;
  };

  var traverse = function traverse(dict) {
    return dict.traverse;
  };

  var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
  }, function () {
    return Data_Maybe.functorMaybe;
  }, function (dictApplicative) {
    return function (v) {
      if (v instanceof Data_Maybe.Nothing) {
        return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Maybe.Just.create)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 90, column 33): " + [v.constructor.name]);
    };
  }, function (dictApplicative) {
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Maybe.Just.create)(v(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 90, column 33): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  });
  exports["Traversable"] = Traversable;
  exports["traverse"] = traverse;
  exports["traversableMaybe"] = traversableMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.TraversableWithIndex"] = $PS["Data.TraversableWithIndex"] || {};
  var exports = $PS["Data.TraversableWithIndex"];

  var TraversableWithIndex = function TraversableWithIndex(FoldableWithIndex1, FunctorWithIndex0, Traversable2, traverseWithIndex) {
    this.FoldableWithIndex1 = FoldableWithIndex1;
    this.FunctorWithIndex0 = FunctorWithIndex0;
    this.Traversable2 = Traversable2;
    this.traverseWithIndex = traverseWithIndex;
  };

  var traverseWithIndex = function traverseWithIndex(dict) {
    return dict.traverseWithIndex;
  };

  exports["TraversableWithIndex"] = TraversableWithIndex;
  exports["traverseWithIndex"] = traverseWithIndex;
})(PS);

(function (exports) {
  "use strict";

  exports["new"] = function () {
    return {};
  };

  exports.poke = function (k) {
    return function (v) {
      return function (m) {
        return function () {
          m[k] = v;
          return m;
        };
      };
    };
  };
})(PS["Foreign.Object.ST"] = PS["Foreign.Object.ST"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Foreign.Object.ST"] = $PS["Foreign.Object.ST"] || {};
  var exports = $PS["Foreign.Object.ST"];
  var $foreign = $PS["Foreign.Object.ST"];
  exports["new"] = $foreign["new"];
  exports["poke"] = $foreign.poke;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Foreign.Object"] = $PS["Foreign.Object"] || {};
  var exports = $PS["Foreign.Object"];
  var $foreign = $PS["Foreign.Object"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_FoldableWithIndex = $PS["Data.FoldableWithIndex"];
  var Data_Function = $PS["Data.Function"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_TraversableWithIndex = $PS["Data.TraversableWithIndex"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Foreign_Object_ST = $PS["Foreign.Object.ST"];
  var values = $foreign.toArrayWithKey(function (v) {
    return function (v1) {
      return v1;
    };
  });
  var thawST = $foreign["_copyST"];

  var mutate = function mutate(f) {
    return function (m) {
      return $foreign.runST(function __do() {
        var v = thawST(m)();
        var v1 = f(v)();
        return v;
      });
    };
  };

  var mapWithKey = function mapWithKey(f) {
    return function (m) {
      return $foreign["_mapWithKey"](m, f);
    };
  };

  var lookup = Data_Function_Uncurried.runFn4($foreign["_lookup"])(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);

  var insert = function insert(k) {
    return function (v) {
      return mutate(Foreign_Object_ST.poke(k)(v));
    };
  };

  var functorObject = new Data_Functor.Functor(function (f) {
    return function (m) {
      return $foreign["_fmapObject"](m, f);
    };
  });
  var functorWithIndexObject = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorObject;
  }, mapWithKey);
  var fold = $foreign["_foldM"](Data_Function.applyFlipped);

  var foldMap = function foldMap(dictMonoid) {
    return function (f) {
      return fold(function (acc) {
        return function (k) {
          return function (v) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(k)(v));
          };
        };
      })(Data_Monoid.mempty(dictMonoid));
    };
  };

  var foldableObject = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return foldMap(dictMonoid)(Data_Function["const"](f));
    };
  }, function (f) {
    return fold(function (z) {
      return function (v) {
        return f(z);
      };
    });
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_Foldable.foldableArray)(f)(z)(values(m));
      };
    };
  });
  var foldableWithIndexObject = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableObject;
  }, function (dictMonoid) {
    return foldMap(dictMonoid);
  }, function (f) {
    return fold(Data_Function.flip(f));
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_Foldable.foldableArray)(Data_Tuple.uncurry(f))(z)($foreign.toArrayWithKey(Data_Tuple.Tuple.create)(m));
      };
    };
  });
  var traversableWithIndexObject = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexObject;
  }, function () {
    return functorWithIndexObject;
  }, function () {
    return traversableObject;
  }, function (dictApplicative) {
    return function (f) {
      return function (ms) {
        return fold(function (acc) {
          return function (k) {
            return function (v) {
              return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Function.flip(insert(k)))(acc))(f(k)(v));
            };
          };
        })(Control_Applicative.pure(dictApplicative)($foreign.empty))(ms);
      };
    };
  });
  var traversableObject = new Data_Traversable.Traversable(function () {
    return foldableObject;
  }, function () {
    return functorObject;
  }, function (dictApplicative) {
    return Data_Traversable.traverse(traversableObject)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  }, function (dictApplicative) {
    var $52 = Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexObject)(dictApplicative);
    return function ($53) {
      return $52(Data_Function["const"]($53));
    };
  });
  exports["insert"] = insert;
  exports["lookup"] = lookup;
  exports["traversableObject"] = traversableObject;
  exports["empty"] = $foreign.empty;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Core"] = $PS["Data.Argonaut.Core"] || {};
  var exports = $PS["Data.Argonaut.Core"];
  var $foreign = $PS["Data.Argonaut.Core"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Foreign_Object = $PS["Foreign.Object"];

  var verbJsonType = function verbJsonType(def) {
    return function (f) {
      return function (g) {
        return g(def)(f);
      };
    };
  };

  var toJsonType = verbJsonType(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
  var jsonEmptyObject = $foreign.fromObject(Foreign_Object.empty);
  var isJsonType = verbJsonType(false)(Data_Function["const"](true));

  var caseJsonString = function caseJsonString(d) {
    return function (f) {
      return function (j) {
        return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), f, Data_Function["const"](d), Data_Function["const"](d), j);
      };
    };
  };

  var caseJsonObject = function caseJsonObject(d) {
    return function (f) {
      return function (j) {
        return $foreign["_caseJson"](Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), f, j);
      };
    };
  };

  var toObject = toJsonType(caseJsonObject);

  var caseJsonNull = function caseJsonNull(d) {
    return function (f) {
      return function (j) {
        return $foreign["_caseJson"](f, Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), j);
      };
    };
  };

  var isNull = isJsonType(caseJsonNull);

  var caseJsonBoolean = function caseJsonBoolean(d) {
    return function (f) {
      return function (j) {
        return $foreign["_caseJson"](Data_Function["const"](d), f, Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), Data_Function["const"](d), j);
      };
    };
  };

  exports["caseJsonBoolean"] = caseJsonBoolean;
  exports["caseJsonString"] = caseJsonString;
  exports["isNull"] = isNull;
  exports["toObject"] = toObject;
  exports["jsonEmptyObject"] = jsonEmptyObject;
  exports["fromString"] = $foreign.fromString;
  exports["fromObject"] = $foreign.fromObject;
  exports["jsonNull"] = $foreign.jsonNull;
  exports["stringify"] = $foreign.stringify;
})(PS);

(function (exports) {
  "use strict";

  exports._jsonParser = function (fail, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail(e.message);
    }
  };
})(PS["Data.Argonaut.Parser"] = PS["Data.Argonaut.Parser"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Parser"] = $PS["Data.Argonaut.Parser"] || {};
  var exports = $PS["Data.Argonaut.Parser"];
  var $foreign = $PS["Data.Argonaut.Parser"];
  var Data_Either = $PS["Data.Either"];

  var jsonParser = function jsonParser(j) {
    return $foreign["_jsonParser"](Data_Either.Left.create, Data_Either.Right.create, j);
  };

  exports["jsonParser"] = jsonParser;
})(PS);

(function (exports) {
  "use strict";

  exports.fromFoldableImpl = function () {
    function Cons(head, tail) {
      this.head = head;
      this.tail = tail;
    }

    var emptyList = {};

    function curryCons(head) {
      return function (tail) {
        return new Cons(head, tail);
      };
    }

    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;

      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }

      return result;
    }

    return function (foldr) {
      return function (xs) {
        return listToArray(foldr(curryCons)(emptyList)(xs));
      };
    };
  }(); //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.length = function (xs) {
    return xs.length;
  }; //------------------------------------------------------------------------------
  // Extending arrays ------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.cons = function (e) {
    return function (l) {
      return [e].concat(l);
    };
  };

  exports.snoc = function (l) {
    return function (e) {
      var l1 = l.slice();
      l1.push(e);
      return l1;
    };
  }; //------------------------------------------------------------------------------
  // Non-indexed reads -----------------------------------------------------------
  //------------------------------------------------------------------------------


  exports["uncons'"] = function (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  }; //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  }; //------------------------------------------------------------------------------
  // Subarrays -------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.slice = function (s) {
    return function (e) {
      return function (l) {
        return l.slice(s, e);
      };
    };
  };

  exports.drop = function (n) {
    return function (l) {
      return n < 1 ? l : l.slice(n);
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Lazy"] = $PS["Control.Lazy"] || {};
  var exports = $PS["Control.Lazy"];

  var Lazy = function Lazy(defer) {
    this.defer = defer;
  };

  var defer = function defer(dict) {
    return dict.defer;
  };

  exports["defer"] = defer;
  exports["Lazy"] = Lazy;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Array"] = $PS["Data.Array"] || {};
  var exports = $PS["Data.Array"];
  var $foreign = $PS["Data.Array"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
      return new Data_Maybe.Just({
        head: x,
        tail: xs
      });
    };
  });
  var tail = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
      return new Data_Maybe.Just(xs);
    };
  });

  var singleton = function singleton(a) {
    return [a];
  };

  var $$null = function $$null(xs) {
    return $foreign.length(xs) === 0;
  };

  var some = function some(dictAlternative) {
    return function (dictLazy) {
      return function (v) {
        return Control_Apply.apply(dictAlternative.Applicative0().Apply0())(Data_Functor.map(dictAlternative.Plus1().Alt0().Functor0())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };

  var many = function many(dictAlternative) {
    return function (dictLazy) {
      return function (v) {
        return Control_Alt.alt(dictAlternative.Plus1().Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([]));
      };
    };
  };

  var init = function init(xs) {
    if ($$null(xs)) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (Data_Boolean.otherwise) {
      return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1 | 0)(xs));
    }

    ;
    throw new Error("Failed pattern match at Data.Array (line 323, column 1 - line 323, column 45): " + [xs.constructor.name]);
  };

  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var last = function last(xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
  };

  var head = function head(xs) {
    return index(xs)(0);
  };

  var fromFoldable = function fromFoldable(dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
  };

  exports["fromFoldable"] = fromFoldable;
  exports["singleton"] = singleton;
  exports["some"] = some;
  exports["head"] = head;
  exports["last"] = last;
  exports["tail"] = tail;
  exports["init"] = init;
  exports["uncons"] = uncons;
  exports["length"] = $foreign.length;
  exports["cons"] = $foreign.cons;
  exports["snoc"] = $foreign.snoc;
  exports["drop"] = $foreign.drop;
})(PS);

(function (exports) {
  "use strict";

  exports.split = function (sep) {
    return function (s) {
      return s.split(sep);
    };
  };

  exports.joinWith = function (s) {
    return function (xs) {
      return xs.join(s);
    };
  };
})(PS["Data.String.Common"] = PS["Data.String.Common"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.String.Common"] = $PS["Data.String.Common"] || {};
  var exports = $PS["Data.String.Common"];
  var $foreign = $PS["Data.String.Common"];

  var $$null = function $$null(s) {
    return s === "";
  };

  exports["null"] = $$null;
  exports["split"] = $foreign.split;
  exports["joinWith"] = $foreign.joinWith;
})(PS);

(function (exports) {
  /* globals exports, JSON */
  "use strict";

  exports.unsafeDecodeURIComponent = decodeURIComponent;
  exports.unsafeEncodeURIComponent = encodeURIComponent;
})(PS["Global.Unsafe"] = PS["Global.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Global.Unsafe"] = $PS["Global.Unsafe"] || {};
  var exports = $PS["Global.Unsafe"];
  var $foreign = $PS["Global.Unsafe"];
  exports["unsafeDecodeURIComponent"] = $foreign.unsafeDecodeURIComponent;
  exports["unsafeEncodeURIComponent"] = $foreign.unsafeEncodeURIComponent;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.FormURLEncoded"] = $PS["Data.FormURLEncoded"] || {};
  var exports = $PS["Data.FormURLEncoded"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Global_Unsafe = $PS["Global.Unsafe"];

  var toArray = function toArray(v) {
    return v;
  };

  var encode = function () {
    var encodePart = function encodePart(v) {
      if (v.value1 instanceof Data_Maybe.Nothing) {
        return Global_Unsafe.unsafeEncodeURIComponent(v.value0);
      }

      ;

      if (v.value1 instanceof Data_Maybe.Just) {
        return Global_Unsafe.unsafeEncodeURIComponent(v.value0) + ("=" + Global_Unsafe.unsafeEncodeURIComponent(v.value1.value0));
      }

      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 35, column 18 - line 37, column 90): " + [v.constructor.name]);
    };

    var $14 = Data_String_Common.joinWith("&");
    var $15 = Data_Functor.map(Data_Functor.functorArray)(encodePart);
    return function ($16) {
      return $14($15(toArray($16)));
    };
  }();

  exports["encode"] = encode;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.HTTP.Method"] = $PS["Data.HTTP.Method"] || {};
  var exports = $PS["Data.HTTP.Method"];
  var Data_Either = $PS["Data.Either"];
  var Data_Show = $PS["Data.Show"];

  var OPTIONS = function () {
    function OPTIONS() {}

    ;
    OPTIONS.value = new OPTIONS();
    return OPTIONS;
  }();

  var GET = function () {
    function GET() {}

    ;
    GET.value = new GET();
    return GET;
  }();

  var HEAD = function () {
    function HEAD() {}

    ;
    HEAD.value = new HEAD();
    return HEAD;
  }();

  var POST = function () {
    function POST() {}

    ;
    POST.value = new POST();
    return POST;
  }();

  var PUT = function () {
    function PUT() {}

    ;
    PUT.value = new PUT();
    return PUT;
  }();

  var DELETE = function () {
    function DELETE() {}

    ;
    DELETE.value = new DELETE();
    return DELETE;
  }();

  var TRACE = function () {
    function TRACE() {}

    ;
    TRACE.value = new TRACE();
    return TRACE;
  }();

  var CONNECT = function () {
    function CONNECT() {}

    ;
    CONNECT.value = new CONNECT();
    return CONNECT;
  }();

  var PROPFIND = function () {
    function PROPFIND() {}

    ;
    PROPFIND.value = new PROPFIND();
    return PROPFIND;
  }();

  var PROPPATCH = function () {
    function PROPPATCH() {}

    ;
    PROPPATCH.value = new PROPPATCH();
    return PROPPATCH;
  }();

  var MKCOL = function () {
    function MKCOL() {}

    ;
    MKCOL.value = new MKCOL();
    return MKCOL;
  }();

  var COPY = function () {
    function COPY() {}

    ;
    COPY.value = new COPY();
    return COPY;
  }();

  var MOVE = function () {
    function MOVE() {}

    ;
    MOVE.value = new MOVE();
    return MOVE;
  }();

  var LOCK = function () {
    function LOCK() {}

    ;
    LOCK.value = new LOCK();
    return LOCK;
  }();

  var UNLOCK = function () {
    function UNLOCK() {}

    ;
    UNLOCK.value = new UNLOCK();
    return UNLOCK;
  }();

  var PATCH = function () {
    function PATCH() {}

    ;
    PATCH.value = new PATCH();
    return PATCH;
  }();

  var unCustomMethod = function unCustomMethod(v) {
    return v;
  };

  var showMethod = new Data_Show.Show(function (v) {
    if (v instanceof OPTIONS) {
      return "OPTIONS";
    }

    ;

    if (v instanceof GET) {
      return "GET";
    }

    ;

    if (v instanceof HEAD) {
      return "HEAD";
    }

    ;

    if (v instanceof POST) {
      return "POST";
    }

    ;

    if (v instanceof PUT) {
      return "PUT";
    }

    ;

    if (v instanceof DELETE) {
      return "DELETE";
    }

    ;

    if (v instanceof TRACE) {
      return "TRACE";
    }

    ;

    if (v instanceof CONNECT) {
      return "CONNECT";
    }

    ;

    if (v instanceof PROPFIND) {
      return "PROPFIND";
    }

    ;

    if (v instanceof PROPPATCH) {
      return "PROPPATCH";
    }

    ;

    if (v instanceof MKCOL) {
      return "MKCOL";
    }

    ;

    if (v instanceof COPY) {
      return "COPY";
    }

    ;

    if (v instanceof MOVE) {
      return "MOVE";
    }

    ;

    if (v instanceof LOCK) {
      return "LOCK";
    }

    ;

    if (v instanceof UNLOCK) {
      return "UNLOCK";
    }

    ;

    if (v instanceof PATCH) {
      return "PATCH";
    }

    ;
    throw new Error("Failed pattern match at Data.HTTP.Method (line 40, column 1 - line 56, column 23): " + [v.constructor.name]);
  });
  var print = Data_Either.either(Data_Show.show(showMethod))(unCustomMethod);
  exports["GET"] = GET;
  exports["POST"] = POST;
  exports["PUT"] = PUT;
  exports["DELETE"] = DELETE;
  exports["print"] = print;
})(PS);

(function (exports) {
  "use strict";

  exports["null"] = null;

  exports.nullable = function (a, r, f) {
    return a == null ? r : f(a);
  };

  exports.notNull = function (x) {
    return x;
  };
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Nullable"] = $PS["Data.Nullable"] || {};
  var exports = $PS["Data.Nullable"];
  var $foreign = $PS["Data.Nullable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);

  var toMaybe = function toMaybe(n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
  };

  exports["toMaybe"] = toMaybe;
  exports["toNullable"] = toNullable;
})(PS);

(function (exports) {
  /* globals setImmediate, clearImmediate, setTimeout, clearTimeout */

  /* jshint -W083, -W098, -W003 */
  "use strict";

  var Aff = function () {
    // A unique value for empty.
    var EMPTY = {};
    /*
    An awkward approximation. We elide evidence we would otherwise need in PS for
    efficiency sake.
    data Aff eff a
    = Pure a
    | Throw Error
    | Catch (Aff eff a) (Error -> Aff eff a)
    | Sync (Eff eff a)
    | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
    | forall b. Bind (Aff eff b) (b -> Aff eff a)
    | forall b. Bracket (Aff eff b) (BracketConditions eff b) (b -> Aff eff a)
    | forall b. Fork Boolean (Aff eff b) ?(Fiber eff b -> a)
    | Sequential (ParAff aff a)
    */

    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    /*
    data ParAff eff a
    = forall b. Map (b -> a) (ParAff eff b)
    | forall b. Apply (ParAff eff (b -> a)) (ParAff eff b)
    | Alt (ParAff eff a) (ParAff eff a)
    | ?Par (Aff eff a)
    */

    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt"; // Various constructors used in interpretation

    var CONS = "Cons"; // Cons-list, for stacks

    var RESUME = "Resume"; // Continue indiscriminately

    var RELEASE = "Release"; // Continue with bracket finalizers

    var FINALIZER = "Finalizer"; // A non-interruptible effect

    var FINALIZED = "Finalized"; // Marker for finalization

    var FORKED = "Forked"; // Reference to a forked fiber, with resumption stack

    var FIBER = "Fiber"; // Actual fiber reference

    var THUNK = "Thunk"; // Primed effect, ready to invoke

    function Aff(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }

    function AffCtr(tag) {
      var fn = function fn(_1, _2, _3) {
        return new Aff(tag, _1, _2, _3);
      };

      fn.tag = tag;
      return fn;
    }

    function nonCanceler(error) {
      return new Aff(PURE, void 0);
    }

    function runEff(eff) {
      try {
        eff();
      } catch (error) {
        setTimeout(function () {
          throw error;
        }, 0);
      }
    }

    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error) {
        return left(error);
      }
    }

    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error) {
        k(left(error))();
        return nonCanceler;
      }
    }

    var Scheduler = function () {
      var limit = 1024;
      var size = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;

      function drain() {
        var thunk;
        draining = true;

        while (size !== 0) {
          size--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }

        draining = false;
      }

      return {
        isDraining: function isDraining() {
          return draining;
        },
        enqueue: function enqueue(cb) {
          var i, tmp;

          if (size === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }

          queue[(ix + size) % limit] = cb;
          size++;

          if (!draining) {
            drain();
          }
        }
      };
    }();

    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function register(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function handler(result) {
              return function () {
                count--;
                delete fibers[fid];
              };
            }
          });
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function isEmpty() {
          return count === 0;
        },
        killAll: function killAll(killError, cb) {
          return function () {
            if (count === 0) {
              return cb();
            }

            var killCount = 0;
            var kills = {};

            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function (result) {
                return function () {
                  delete kills[fid];
                  killCount--;

                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function () {
                      throw util.fromLeft(result);
                    }, 0);
                  }

                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }

            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }

            fibers = {};
            fiberId = 0;
            count = 0;
            return function (error) {
              return new Aff(SYNC, function () {
                for (var k in kills) {
                  if (kills.hasOwnProperty(k)) {
                    kills[k]();
                  }
                }
              });
            };
          };
        }
      };
    } // Fiber state machine


    var SUSPENDED = 0; // Suspended, pending a join.

    var CONTINUE = 1; // Interpret the next instruction.

    var STEP_BIND = 2; // Apply the next bind.

    var STEP_RESULT = 3; // Handle potential failure from a result.

    var PENDING = 4; // An async effect is running.

    var RETURN = 5; // The current stack has returned.

    var COMPLETED = 6; // The entire fiber has completed.

    function Fiber(util, supervisor, aff) {
      // Monotonically increasing tick, increased on each asynchronous turn.
      var runTick = 0; // The current branch of the state machine.

      var status = SUSPENDED; // The current point of interest for the state machine branch.

      var step = aff; // Successful step

      var fail = null; // Failure step

      var interrupt = null; // Asynchronous interrupt
      // Stack of continuations for the current fiber.

      var bhead = null;
      var btail = null; // Stack of attempts and finalizers for error recovery. Every `Cons` is also
      // tagged with current `interrupt` state. We use this to track which items
      // should be ignored or evaluated as a result of a kill.

      var attempts = null; // A special state is needed for Bracket, because it cannot be killed. When
      // we enter a bracket acquisition or finalizer, we increment the counter,
      // and then decrement once complete.

      var bracketCount = 0; // Each join gets a new id so they can be revoked.

      var joinId = 0;
      var joins = null;
      var rethrow = true; // Each invocation of `run` requires a tick. When an asynchronous effect is
      // resolved, we must check that the local tick coincides with the fiber
      // tick before resuming. This prevents multiple async continuations from
      // accidentally resuming the same fiber. A common example may be invoking
      // the provided callback in `makeAff` more than once, but it may also be an
      // async effect resuming after the fiber was already cancelled.

      function _run(localRunTick) {
        var tmp, result, attempt;

        while (true) {
          tmp = null;
          result = null;
          attempt = null;

          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              step = bhead(step);

              if (btail === null) {
                bhead = null;
              } else {
                bhead = btail._1;
                btail = btail._2;
              }

              break;

            case STEP_RESULT:
              if (util.isLeft(step)) {
                status = RETURN;
                fail = step;
                step = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step = util.fromRight(step);
              }

              break;

            case CONTINUE:
              switch (step.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff(CONS, bhead, btail);
                  }

                  bhead = step._2;
                  status = CONTINUE;
                  step = step._1;
                  break;

                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step = util.right(step._1);
                  } else {
                    status = STEP_BIND;
                    step = step._1;
                  }

                  break;

                case SYNC:
                  status = STEP_RESULT;
                  step = runSync(util.left, util.right, step._1);
                  break;

                case ASYNC:
                  status = PENDING;
                  step = runAsync(util.left, step._1, function (result) {
                    return function () {
                      if (runTick !== localRunTick) {
                        return;
                      }

                      runTick++;
                      Scheduler.enqueue(function () {
                        // It's possible to interrupt the fiber between enqueuing and
                        // resuming, so we need to check that the runTick is still
                        // valid.
                        if (runTick !== localRunTick + 1) {
                          return;
                        }

                        status = STEP_RESULT;
                        step = result;

                        _run(runTick);
                      });
                    };
                  });
                  return;

                case THROW:
                  status = RETURN;
                  fail = util.left(step._1);
                  step = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.

                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }

                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.

                case BRACKET:
                  bracketCount++;

                  if (bhead === null) {
                    attempts = new Aff(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }

                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;

                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step._2);

                  if (supervisor) {
                    supervisor.register(tmp);
                  }

                  if (step._1) {
                    tmp.run();
                  }

                  step = util.right(tmp);
                  break;

                case SEQ:
                  status = CONTINUE;
                  step = sequential(util, supervisor, step._1);
                  break;
              }

              break;

            case RETURN:
              bhead = null;
              btail = null; // If the current stack has returned, and we have no other stacks to
              // resume or finalizers to run, the fiber has halted and we can
              // invoke all join callbacks. Otherwise we need to resume.

              if (attempts === null) {
                status = COMPLETED;
                step = interrupt || fail || step;
              } else {
                // The interrupt status for the enqueued item.
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;

                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    // We should compare the interrupt status as well because we
                    // only want it to apply if there has been an interrupt since
                    // enqueuing the catch.
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail) {
                      status = CONTINUE;
                      step = attempt._2(util.fromLeft(fail));
                      fail = null;
                    }

                    break;
                  // We cannot resume from an unmasked interrupt or exception.

                  case RESUME:
                    // As with Catch, we only want to ignore in the case of an
                    // interrupt since enqueing the item.
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step = util.fromRight(step);
                    }

                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.

                  case BRACKET:
                    bracketCount--;

                    if (fail === null) {
                      result = util.fromRight(step); // We need to enqueue the Release with the same interrupt
                      // status as the Bracket that is initiating it.

                      attempts = new Aff(CONS, new Aff(RELEASE, attempt._2, result), attempts, tmp); // We should only coninue as long as the interrupt status has not changed or
                      // we are currently within a non-interruptable finalizer.

                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step = attempt._3(result);
                      }
                    }

                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.

                  case RELEASE:
                    attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE; // It has only been killed if the interrupt status has changed
                    // since we enqueued the item, and the bracket count is 0. If the
                    // bracket count is non-zero then we are in a masked state so it's
                    // impossible to be killed.

                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail) {
                      step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
                    } else {
                      step = attempt._1.completed(util.fromRight(step))(attempt._2);
                    }

                    fail = null;
                    bracketCount++;
                    break;

                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;

                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail = attempt._2;
                    break;
                }
              }

              break;

            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step));
                }
              }

              joins = null; // If we have an interrupt and a fail, then the thread threw while
              // running finalizers. This should always rethrow in a fresh stack.

              if (interrupt && fail) {
                setTimeout(function () {
                  throw util.fromLeft(fail);
                }, 0); // If we have an unhandled exception, and no other fiber has joined
                // then we need to throw the exception in a fresh stack.
              } else if (util.isLeft(step) && rethrow) {
                setTimeout(function () {
                  // Guard on reathrow because a completely synchronous fiber can
                  // still have an observer which was added after-the-fact.
                  if (rethrow) {
                    throw util.fromLeft(step);
                  }
                }, 0);
              }

              return;

            case SUSPENDED:
              status = CONTINUE;
              break;

            case PENDING:
              return;
          }
        }
      }

      function onComplete(join) {
        return function () {
          if (status === COMPLETED) {
            rethrow = rethrow && join.rethrow;
            join.handler(step)();
            return function () {};
          }

          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join;
          return function () {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }

      function kill(error, cb) {
        return function () {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function () {};
          }

          var canceler = onComplete({
            rethrow: false,
            handler: function handler()
            /* unused */
            {
              return cb(util.right(void 0));
            }
          })();

          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error);
              status = COMPLETED;
              step = interrupt;

              _run(runTick);

              break;

            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error);
              }

              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff(CONS, new Aff(FINALIZER, step(error)), attempts, interrupt);
                }

                status = RETURN;
                step = null;
                fail = null;

                _run(++runTick);
              }

              break;

            default:
              if (interrupt === null) {
                interrupt = util.left(error);
              }

              if (bracketCount === 0) {
                status = RETURN;
                step = null;
                fail = null;
              }

          }

          return canceler;
        };
      }

      function join(cb) {
        return function () {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();

          if (status === SUSPENDED) {
            _run(runTick);
          }

          return canceler;
        };
      }

      return {
        kill: kill,
        join: join,
        onComplete: onComplete,
        isSuspended: function isSuspended() {
          return status === SUSPENDED;
        },
        run: function run() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function () {
                _run(runTick);
              });
            } else {
              _run(runTick);
            }
          }
        }
      };
    }

    function runPar(util, supervisor, par, cb) {
      // Table of all forked fibers.
      var fiberId = 0;
      var fibers = {}; // Table of currently running cancelers, as a product of `Alt` behavior.

      var killId = 0;
      var kills = {}; // Error used for early cancelation on Alt branches.

      var early = new Error("[ParAff] Early exit"); // Error used to kill the entire tree.

      var interrupt = null; // The root pointer of the tree.

      var root = EMPTY; // Walks a tree, invoking all the cancelers. Returns the table of pending
      // cancellation fibers.

      function kill(error, par, cb) {
        var step = par;
        var head = null;
        var tail = null;
        var count = 0;
        var kills = {};
        var tmp, kid;

        loop: while (true) {
          tmp = null;

          switch (step.tag) {
            case FORKED:
              if (step._3 === EMPTY) {
                tmp = fibers[step._1];
                kills[count++] = tmp.kill(error, function (result) {
                  return function () {
                    count--;

                    if (count === 0) {
                      cb(result)();
                    }
                  };
                });
              } // Terminal case.


              if (head === null) {
                break loop;
              } // Go down the right side of the tree.


              step = head._2;

              if (tail === null) {
                head = null;
              } else {
                head = tail._1;
                tail = tail._2;
              }

              break;

            case MAP:
              step = step._2;
              break;

            case APPLY:
            case ALT:
              if (head) {
                tail = new Aff(CONS, head, tail);
              }

              head = step;
              step = step._1;
              break;
          }
        }

        if (count === 0) {
          cb(util.right(void 0))();
        } else {
          // Run the cancelation effects. We alias `count` because it's mutable.
          kid = 0;
          tmp = count;

          for (; kid < tmp; kid++) {
            kills[kid] = kills[kid]();
          }
        }

        return kills;
      } // When a fiber resolves, we need to bubble back up the tree with the
      // result, computing the applicative nodes.


      function join(result, head, tail) {
        var fail, step, lhs, rhs, tmp, kid;

        if (util.isLeft(result)) {
          fail = result;
          step = null;
        } else {
          step = result;
          fail = null;
        }

        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null; // We should never continue if the entire tree has been interrupted.

          if (interrupt !== null) {
            return;
          } // We've made it all the way to the root of the tree, which means
          // the tree has fully evaluated.


          if (head === null) {
            cb(fail || step)();
            return;
          } // The tree has already been computed, so we shouldn't try to do it
          // again. This should never happen.
          // TODO: Remove this?


          if (head._3 !== EMPTY) {
            return;
          }

          switch (head.tag) {
            case MAP:
              if (fail === null) {
                head._3 = util.right(head._1(util.fromRight(step)));
                step = head._3;
              } else {
                head._3 = fail;
              }

              break;

            case APPLY:
              lhs = head._1._3;
              rhs = head._2._3; // If we have a failure we should kill the other side because we
              // can't possible yield a result anymore.

              if (fail) {
                head._3 = fail;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail === lhs ? head._2 : head._1, function ()
                /* unused */
                {
                  return function () {
                    delete kills[kid];

                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join(fail, null, null);
                    } else {
                      join(fail, tail._1, tail._2);
                    }
                  };
                });

                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                // We can only proceed if both sides have resolved.
                return;
              } else {
                step = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head._3 = step;
              }

              break;

            case ALT:
              lhs = head._1._3;
              rhs = head._2._3; // We can only proceed if both have resolved or we have a success

              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              } // If both sides resolve with an error, we should continue with the
              // first error


              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail = step === lhs ? rhs : lhs;
                step = null;
                head._3 = fail;
              } else {
                head._3 = step;
                tmp = true;
                kid = killId++; // Once a side has resolved, we need to cancel the side that is still
                // pending before we can continue.

                kills[kid] = kill(early, step === lhs ? head._2 : head._1, function ()
                /* unused */
                {
                  return function () {
                    delete kills[kid];

                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join(step, null, null);
                    } else {
                      join(step, tail._1, tail._2);
                    }
                  };
                });

                if (tmp) {
                  tmp = false;
                  return;
                }
              }

              break;
          }

          if (tail === null) {
            head = null;
          } else {
            head = tail._1;
            tail = tail._2;
          }
        }
      }

      function resolve(fiber) {
        return function (result) {
          return function () {
            delete fibers[fiber._1];
            fiber._3 = result;
            join(result, fiber._2._1, fiber._2._2);
          };
        };
      } // Walks the applicative tree, substituting non-applicative nodes with
      // `FORKED` nodes. In this tree, all applicative nodes use the `_3` slot
      // as a mutable slot for memoization. In an unresolved state, the `_3`
      // slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
      // the left side first, because both operations are left-associative. As
      // we `RETURN` from those branches, we then walk the right side.


      function run() {
        var status = CONTINUE;
        var step = par;
        var head = null;
        var tail = null;
        var tmp, fid;

        loop: while (true) {
          tmp = null;
          fid = null;

          switch (status) {
            case CONTINUE:
              switch (step.tag) {
                case MAP:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;

                case APPLY:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;

                case ALT:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;

                default:
                  // When we hit a leaf value, we suspend the stack in the `FORKED`.
                  // When the fiber resolves, it can bubble back up the tree.
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff(FORKED, fid, new Aff(CONS, head, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step)
                  })();
                  fibers[fid] = tmp;

                  if (supervisor) {
                    supervisor.register(tmp);
                  }

              }

              break;

            case RETURN:
              // Terminal case, we are back at the root.
              if (head === null) {
                break loop;
              } // If we are done with the right side, we need to continue down the
              // left. Otherwise we should continue up the stack.


              if (head._1 === EMPTY) {
                head._1 = step;
                status = CONTINUE;
                step = head._2;
                head._2 = EMPTY;
              } else {
                head._2 = step;
                step = head;

                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
              }

          }
        } // Keep a reference to the tree root so it can be cancelled.


        root = step;

        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      } // Cancels the entire tree. If there are already subtrees being canceled,
      // we need to first cancel those joins. We will then add fresh joins for
      // all pending branches including those that were in the process of being
      // canceled.


      function cancel(error, cb) {
        interrupt = util.left(error);
        var innerKills;

        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];

            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }

        kills = null;
        var newKills = kill(error, root, cb);
        return function (killError) {
          return new Aff(ASYNC, function (killCb) {
            return function () {
              for (var kid in newKills) {
                if (newKills.hasOwnProperty(kid)) {
                  newKills[kid]();
                }
              }

              return nonCanceler;
            };
          });
        };
      }

      run();
      return function (killError) {
        return new Aff(ASYNC, function (killCb) {
          return function () {
            return cancel(killError, killCb);
          };
        });
      };
    }

    function sequential(util, supervisor, par) {
      return new Aff(ASYNC, function (cb) {
        return function () {
          return runPar(util, supervisor, par, cb);
        };
      });
    }

    Aff.EMPTY = EMPTY;
    Aff.Pure = AffCtr(PURE);
    Aff.Throw = AffCtr(THROW);
    Aff.Catch = AffCtr(CATCH);
    Aff.Sync = AffCtr(SYNC);
    Aff.Async = AffCtr(ASYNC);
    Aff.Bind = AffCtr(BIND);
    Aff.Bracket = AffCtr(BRACKET);
    Aff.Fork = AffCtr(FORK);
    Aff.Seq = AffCtr(SEQ);
    Aff.ParMap = AffCtr(MAP);
    Aff.ParApply = AffCtr(APPLY);
    Aff.ParAlt = AffCtr(ALT);
    Aff.Fiber = Fiber;
    Aff.Supervisor = Supervisor;
    Aff.Scheduler = Scheduler;
    Aff.nonCanceler = nonCanceler;
    return Aff;
  }();

  exports._pure = Aff.Pure;
  exports._throwError = Aff.Throw;

  exports._catchError = function (aff) {
    return function (k) {
      return Aff.Catch(aff, k);
    };
  };

  exports._map = function (f) {
    return function (aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function (value) {
          return Aff.Pure(f(value));
        });
      }
    };
  };

  exports._bind = function (aff) {
    return function (k) {
      return Aff.Bind(aff, k);
    };
  };

  exports._fork = function (immediate) {
    return function (aff) {
      return Aff.Fork(immediate, aff);
    };
  };

  exports._liftEffect = Aff.Sync;

  exports._parAffMap = function (f) {
    return function (aff) {
      return Aff.ParMap(f, aff);
    };
  };

  exports._parAffApply = function (aff1) {
    return function (aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  };

  exports.makeAff = Aff.Async;

  exports.generalBracket = function (acquire) {
    return function (options) {
      return function (k) {
        return Aff.Bracket(acquire, options, k);
      };
    };
  };

  exports._makeFiber = function (util, aff) {
    return function () {
      return Aff.Fiber(util, null, aff);
    };
  };

  exports._delay = function () {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }

    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }

    return function (right, ms) {
      return Aff.Async(function (cb) {
        return function () {
          var timer = setDelay(ms, cb(right()));
          return function () {
            return Aff.Sync(function () {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();

  exports._sequential = Aff.Seq;
})(PS["Effect.Aff"] = PS["Effect.Aff"] || {});

(function (exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var monadEffect = new Control_Monad.Monad(function () {
    return applicativeEffect;
  }, function () {
    return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
    return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
    return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
    return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applyEffect"] = applyEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS);

(function (exports) {
  "use strict";

  exports.new = function (val) {
    return function () {
      return {
        value: val
      };
    };
  };

  exports.read = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports["modify'"] = function (f) {
    return function (ref) {
      return function () {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };

  exports.write = function (val) {
    return function (ref) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Ref"] = $PS["Effect.Ref"] || {};
  var exports = $PS["Effect.Ref"];
  var $foreign = $PS["Effect.Ref"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect = $PS["Effect"];

  var modify = function modify(f) {
    return $foreign["modify'"](function (s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };

  var modify_ = function modify_(f) {
    return function (s) {
      return Data_Functor["void"](Effect.functorEffect)(modify(f)(s));
    };
  };

  exports["modify_"] = modify_;
  exports["new"] = $foreign["new"];
  exports["read"] = $foreign.read;
  exports["modify'"] = $foreign["modify'"];
  exports["write"] = $foreign.write;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Rec.Class"] = $PS["Control.Monad.Rec.Class"] || {};
  var exports = $PS["Control.Monad.Rec.Class"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Ref = $PS["Effect.Ref"];

  var Loop = function () {
    function Loop(value0) {
      this.value0 = value0;
    }

    ;

    Loop.create = function (value0) {
      return new Loop(value0);
    };

    return Loop;
  }();

  var Done = function () {
    function Done(value0) {
      this.value0 = value0;
    }

    ;

    Done.create = function (value0) {
      return new Done(value0);
    };

    return Done;
  }();

  var MonadRec = function MonadRec(Monad0, tailRecM) {
    this.Monad0 = Monad0;
    this.tailRecM = tailRecM;
  };

  var tailRecM = function tailRecM(dict) {
    return dict.tailRecM;
  };

  var monadRecEffect = new MonadRec(function () {
    return Effect.monadEffect;
  }, function (f) {
    return function (a) {
      var fromDone = function fromDone(v) {
        if (v instanceof Done) {
          return v.value0;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 111, column 30 - line 111, column 44): " + [v.constructor.name]);
      };

      return function __do() {
        var v = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref["new"])(f(a))();

        (function () {
          while (!function __do() {
            var v1 = Effect_Ref.read(v)();

            if (v1 instanceof Loop) {
              var v2 = f(v1.value0)();
              var v3 = Effect_Ref.write(v2)(v)();
              return false;
            }

            ;

            if (v1 instanceof Done) {
              return true;
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 102, column 22 - line 107, column 28): " + [v1.constructor.name]);
          }()) {}

          ;
          return {};
        })();

        return Data_Functor.map(Effect.functorEffect)(fromDone)(Effect_Ref.read(v))();
      };
    };
  });

  var forever = function forever(dictMonadRec) {
    return function (ma) {
      return tailRecM(dictMonadRec)(function (u) {
        return Data_Functor.voidRight(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(new Loop(u))(ma);
      })(Data_Unit.unit);
    };
  };

  exports["Loop"] = Loop;
  exports["Done"] = Done;
  exports["MonadRec"] = MonadRec;
  exports["tailRecM"] = tailRecM;
  exports["forever"] = forever;
  exports["monadRecEffect"] = monadRecEffect;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Parallel.Class"] = $PS["Control.Parallel.Class"] || {};
  var exports = $PS["Control.Parallel.Class"];

  var Parallel = function Parallel(Applicative1, Monad0, parallel, sequential) {
    this.Applicative1 = Applicative1;
    this.Monad0 = Monad0;
    this.parallel = parallel;
    this.sequential = sequential;
  };

  var sequential = function sequential(dict) {
    return dict.sequential;
  };

  var parallel = function parallel(dict) {
    return dict.parallel;
  };

  exports["parallel"] = parallel;
  exports["sequential"] = sequential;
  exports["Parallel"] = Parallel;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Class"] = $PS["Effect.Class"] || {};
  var exports = $PS["Effect.Class"];
  var Control_Category = $PS["Control.Category"];
  var Effect = $PS["Effect"];

  var MonadEffect = function MonadEffect(Monad0, liftEffect) {
    this.Monad0 = Monad0;
    this.liftEffect = liftEffect;
  };

  var monadEffectEffect = new MonadEffect(function () {
    return Effect.monadEffect;
  }, Control_Category.identity(Control_Category.categoryFn));

  var liftEffect = function liftEffect(dict) {
    return dict.liftEffect;
  };

  exports["liftEffect"] = liftEffect;
  exports["MonadEffect"] = MonadEffect;
  exports["monadEffectEffect"] = monadEffectEffect;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafePerformEffect = function (f) {
    return f();
  };
})(PS["Effect.Unsafe"] = PS["Effect.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Unsafe"] = $PS["Effect.Unsafe"] || {};
  var exports = $PS["Effect.Unsafe"];
  var $foreign = $PS["Effect.Unsafe"];
  exports["unsafePerformEffect"] = $foreign.unsafePerformEffect;
})(PS);

(function (exports) {
  "use strict"; // module Partial.Unsafe

  exports.unsafePartial = function (f) {
    return f();
  };
})(PS["Partial.Unsafe"] = PS["Partial.Unsafe"] || {});

(function (exports) {
  "use strict"; // module Partial

  exports.crashWith = function () {
    return function (msg) {
      throw new Error(msg);
    };
  };
})(PS["Partial"] = PS["Partial"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Partial"] = $PS["Partial"] || {};
  var exports = $PS["Partial"];
  var $foreign = $PS["Partial"];
  exports["crashWith"] = $foreign.crashWith;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Partial.Unsafe"] = $PS["Partial.Unsafe"] || {};
  var exports = $PS["Partial.Unsafe"];
  var $foreign = $PS["Partial.Unsafe"];
  var Partial = $PS["Partial"];

  var unsafeCrashWith = function unsafeCrashWith(msg) {
    return $foreign.unsafePartial(function (dictPartial) {
      return Partial.crashWith()(msg);
    });
  };

  exports["unsafeCrashWith"] = unsafeCrashWith;
})(PS);

(function (exports) {
  "use strict"; // module Unsafe.Coerce

  exports.unsafeCoerce = function (x) {
    return x;
  };
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Unsafe.Coerce"] = $PS["Unsafe.Coerce"] || {};
  var exports = $PS["Unsafe.Coerce"];
  var $foreign = $PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Aff"] = $PS["Effect.Aff"] || {};
  var exports = $PS["Effect.Aff"];
  var $foreign = $PS["Effect.Aff"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Unsafe = $PS["Effect.Unsafe"];
  var Partial_Unsafe = $PS["Partial.Unsafe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Canceler = function Canceler(x) {
    return x;
  };

  var suspendAff = $foreign["_fork"](false);
  var functorParAff = new Data_Functor.Functor($foreign["_parAffMap"]);
  var functorAff = new Data_Functor.Functor($foreign["_map"]);
  var forkAff = $foreign["_fork"](true);

  var ffiUtil = function () {
    var unsafeFromRight = function unsafeFromRight(v) {
      if (v instanceof Data_Either.Right) {
        return v.value0;
      }

      ;

      if (v instanceof Data_Either.Left) {
        return Partial_Unsafe.unsafeCrashWith("unsafeFromRight: Left");
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 400, column 21 - line 402, column 54): " + [v.constructor.name]);
    };

    var unsafeFromLeft = function unsafeFromLeft(v) {
      if (v instanceof Data_Either.Left) {
        return v.value0;
      }

      ;

      if (v instanceof Data_Either.Right) {
        return Partial_Unsafe.unsafeCrashWith("unsafeFromLeft: Right");
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 395, column 20 - line 397, column 54): " + [v.constructor.name]);
    };

    var isLeft = function isLeft(v) {
      if (v instanceof Data_Either.Left) {
        return true;
      }

      ;

      if (v instanceof Data_Either.Right) {
        return false;
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 390, column 12 - line 392, column 20): " + [v.constructor.name]);
    };

    return {
      isLeft: isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Data_Either.Left.create,
      right: Data_Either.Right.create
    };
  }();

  var makeFiber = function makeFiber(aff) {
    return $foreign["_makeFiber"](ffiUtil, aff);
  };

  var launchAff = function launchAff(aff) {
    return function __do() {
      var v = makeFiber(aff)();
      v.run();
      return v;
    };
  };

  var launchAff_ = function () {
    var $49 = Data_Functor["void"](Effect.functorEffect);
    return function ($50) {
      return $49(launchAff($50));
    };
  }();

  var delay = function delay(v) {
    return $foreign["_delay"](Data_Either.Right.create, v);
  };

  var bracket = function bracket(acquire) {
    return function (completed) {
      return $foreign.generalBracket(acquire)({
        killed: Data_Function["const"](completed),
        failed: Data_Function["const"](completed),
        completed: Data_Function["const"](completed)
      });
    };
  };

  var applyParAff = new Control_Apply.Apply(function () {
    return functorParAff;
  }, $foreign["_parAffApply"]);
  var monadAff = new Control_Monad.Monad(function () {
    return applicativeAff;
  }, function () {
    return bindAff;
  });
  var bindAff = new Control_Bind.Bind(function () {
    return applyAff;
  }, $foreign["_bind"]);
  var applyAff = new Control_Apply.Apply(function () {
    return functorAff;
  }, Control_Monad.ap(monadAff));
  var applicativeAff = new Control_Applicative.Applicative(function () {
    return applyAff;
  }, $foreign["_pure"]);

  var $$finally = function $$finally(fin) {
    return function (a) {
      return bracket(Control_Applicative.pure(applicativeAff)(Data_Unit.unit))(Data_Function["const"](fin))(Data_Function["const"](a));
    };
  };

  var monadEffectAff = new Effect_Class.MonadEffect(function () {
    return monadAff;
  }, $foreign["_liftEffect"]);

  var effectCanceler = function () {
    var $51 = Effect_Class.liftEffect(monadEffectAff);
    return function ($52) {
      return Canceler(Data_Function["const"]($51($52)));
    };
  }();

  var joinFiber = function joinFiber(v) {
    return $foreign.makeAff(function (k) {
      return Data_Functor.map(Effect.functorEffect)(effectCanceler)(v.join(k));
    });
  };

  var functorFiber = new Data_Functor.Functor(function (f) {
    return function (t) {
      return Effect_Unsafe.unsafePerformEffect(makeFiber(Data_Functor.map(functorAff)(f)(joinFiber(t))));
    };
  });

  var killFiber = function killFiber(e) {
    return function (v) {
      return Control_Bind.bind(bindAff)(Effect_Class.liftEffect(monadEffectAff)(v.isSuspended))(function (v1) {
        if (v1) {
          return Effect_Class.liftEffect(monadEffectAff)(Data_Functor["void"](Effect.functorEffect)(v.kill(e, Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit)))));
        }

        ;
        return $foreign.makeAff(function (k) {
          return Data_Functor.map(Effect.functorEffect)(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };

  var monadThrowAff = new Control_Monad_Error_Class.MonadThrow(function () {
    return monadAff;
  }, $foreign["_throwError"]);
  var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
    return monadThrowAff;
  }, $foreign["_catchError"]);
  var attempt = Control_Monad_Error_Class["try"](monadErrorAff);

  var runAff = function runAff(k) {
    return function (aff) {
      return launchAff(Control_Bind.bindFlipped(bindAff)(function () {
        var $55 = Effect_Class.liftEffect(monadEffectAff);
        return function ($56) {
          return $55(k($56));
        };
      }())(Control_Monad_Error_Class["try"](monadErrorAff)(aff)));
    };
  };

  var runAff_ = function runAff_(k) {
    return function (aff) {
      return Data_Functor["void"](Effect.functorEffect)(runAff(k)(aff));
    };
  };

  var parallelAff = new Control_Parallel_Class.Parallel(function () {
    return applicativeParAff;
  }, function () {
    return monadAff;
  }, Unsafe_Coerce.unsafeCoerce, $foreign["_sequential"]);
  var applicativeParAff = new Control_Applicative.Applicative(function () {
    return applyParAff;
  }, function () {
    var $59 = Control_Parallel_Class.parallel(parallelAff);
    var $60 = Control_Applicative.pure(applicativeAff);
    return function ($61) {
      return $59($60($61));
    };
  }());
  var monadRecAff = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadAff;
  }, function (k) {
    var go = function go(a) {
      return Control_Bind.bind(bindAff)(k(a))(function (v) {
        if (v instanceof Control_Monad_Rec_Class.Done) {
          return Control_Applicative.pure(applicativeAff)(v.value0);
        }

        ;

        if (v instanceof Control_Monad_Rec_Class.Loop) {
          return go(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Effect.Aff (line 100, column 7 - line 102, column 22): " + [v.constructor.name]);
      });
    };

    return go;
  });
  var nonCanceler = Data_Function["const"](Control_Applicative.pure(applicativeAff)(Data_Unit.unit));
  exports["launchAff_"] = launchAff_;
  exports["runAff_"] = runAff_;
  exports["forkAff"] = forkAff;
  exports["suspendAff"] = suspendAff;
  exports["attempt"] = attempt;
  exports["delay"] = delay;
  exports["finally"] = $$finally;
  exports["killFiber"] = killFiber;
  exports["joinFiber"] = joinFiber;
  exports["nonCanceler"] = nonCanceler;
  exports["effectCanceler"] = effectCanceler;
  exports["functorAff"] = functorAff;
  exports["applicativeAff"] = applicativeAff;
  exports["bindAff"] = bindAff;
  exports["monadAff"] = monadAff;
  exports["monadRecAff"] = monadRecAff;
  exports["monadThrowAff"] = monadThrowAff;
  exports["monadErrorAff"] = monadErrorAff;
  exports["monadEffectAff"] = monadEffectAff;
  exports["applicativeParAff"] = applicativeParAff;
  exports["parallelAff"] = parallelAff;
  exports["functorFiber"] = functorFiber;
  exports["makeAff"] = $foreign.makeAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Aff.Compat"] = $PS["Effect.Aff.Compat"] || {};
  var exports = $PS["Effect.Aff.Compat"];
  var Data_Either = $PS["Data.Either"];
  var Effect_Aff = $PS["Effect.Aff"];

  var fromEffectFnAff = function fromEffectFnAff(v) {
    return Effect_Aff.makeAff(function (k) {
      return function __do() {
        var v1 = v(function ($4) {
          return k(Data_Either.Left.create($4))();
        }, function ($5) {
          return k(Data_Either.Right.create($5))();
        });
        return function (e) {
          return Effect_Aff.makeAff(function (k2) {
            return function __do() {
              v1(e, function ($6) {
                return k2(Data_Either.Left.create($6))();
              }, function ($7) {
                return k2(Data_Either.Right.create($7))();
              });
              return Effect_Aff.nonCanceler;
            };
          });
        };
      };
    });
  };

  exports["fromEffectFnAff"] = fromEffectFnAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Affjax"] = $PS["Affjax"] || {};
  var exports = $PS["Affjax"];
  var $foreign = $PS["Affjax"];
  var Affjax_RequestBody = $PS["Affjax.RequestBody"];
  var Affjax_RequestHeader = $PS["Affjax.RequestHeader"];
  var Affjax_ResponseFormat = $PS["Affjax.ResponseFormat"];
  var Affjax_ResponseHeader = $PS["Affjax.ResponseHeader"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Argonaut_Parser = $PS["Data.Argonaut.Parser"];
  var Data_Array = $PS["Data.Array"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_FormURLEncoded = $PS["Data.FormURLEncoded"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HTTP_Method = $PS["Data.HTTP.Method"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_Compat = $PS["Effect.Aff.Compat"];
  var Foreign = $PS["Foreign"];

  var request = function request(req) {
    var parseJSON = function parseJSON(v) {
      if (v === "") {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Argonaut_Core.jsonEmptyObject);
      }

      ;
      return Data_Either.either(function ($66) {
        return Foreign.fail(Foreign.ForeignError.create($66));
      })(Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity)))(Data_Argonaut_Parser.jsonParser(v));
    };

    var fromResponse$prime = function () {
      if (req.responseFormat instanceof Affjax_ResponseFormat["ArrayBuffer"]) {
        return Foreign.unsafeReadTagged("ArrayBuffer");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Blob) {
        return Foreign.unsafeReadTagged("Blob");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Document) {
        return Foreign.unsafeReadTagged("Document");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Json) {
        return Control_Bind.composeKleisliFlipped(Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity))(function ($67) {
          return req.responseFormat.value0(parseJSON($67));
        })(Foreign.unsafeReadTagged("String"));
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat["String"]) {
        return Foreign.unsafeReadTagged("String");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Ignore) {
        return Data_Function["const"](req.responseFormat.value0(Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Unit.unit)));
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 294, column 19 - line 300, column 57): " + [req.responseFormat.constructor.name]);
    }();

    var extractContent = function extractContent(v) {
      if (v instanceof Affjax_RequestBody.ArrayView) {
        return v.value0(Foreign.unsafeToForeign);
      }

      ;

      if (v instanceof Affjax_RequestBody.Blob) {
        return Foreign.unsafeToForeign(v.value0);
      }

      ;

      if (v instanceof Affjax_RequestBody.Document) {
        return Foreign.unsafeToForeign(v.value0);
      }

      ;

      if (v instanceof Affjax_RequestBody["String"]) {
        return Foreign.unsafeToForeign(v.value0);
      }

      ;

      if (v instanceof Affjax_RequestBody.FormData) {
        return Foreign.unsafeToForeign(v.value0);
      }

      ;

      if (v instanceof Affjax_RequestBody.FormURLEncoded) {
        return Foreign.unsafeToForeign(Data_FormURLEncoded.encode(v.value0));
      }

      ;

      if (v instanceof Affjax_RequestBody.Json) {
        return Foreign.unsafeToForeign(Data_Argonaut_Core.stringify(v.value0));
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 268, column 20 - line 275, column 57): " + [v.constructor.name]);
    };

    var addHeader = function addHeader(mh) {
      return function (hs) {
        if (mh instanceof Data_Maybe.Just && !Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Function.on(Data_Eq.eq(Data_Eq.eqString))(Affjax_RequestHeader.name)(mh.value0))(hs)) {
          return Data_Array.snoc(hs)(mh.value0);
        }

        ;
        return hs;
      };
    };

    var headers = function headers(reqContent) {
      return addHeader(Data_Functor.map(Data_Maybe.functorMaybe)(Affjax_RequestHeader.ContentType.create)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Affjax_RequestBody.toMediaType)(reqContent)))(addHeader(Data_Functor.map(Data_Maybe.functorMaybe)(Affjax_RequestHeader.Accept.create)(Affjax_ResponseFormat.toMediaType(req.responseFormat)))(req.headers));
    };

    var req$prime = {
      method: Data_HTTP_Method.print(req.method),
      url: req.url,
      headers: Data_Functor.map(Data_Functor.functorArray)(function (h) {
        return {
          field: Affjax_RequestHeader.name(h),
          value: Affjax_RequestHeader.value(h)
        };
      })(headers(req.content)),
      content: Data_Nullable.toNullable(Data_Functor.map(Data_Maybe.functorMaybe)(extractContent)(req.content)),
      responseType: Affjax_ResponseFormat.toResponseType(req.responseFormat),
      username: Data_Nullable.toNullable(req.username),
      password: Data_Nullable.toNullable(req.password),
      withCredentials: req.withCredentials
    };
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_Compat.fromEffectFnAff($foreign["_ajax"](Affjax_ResponseHeader.ResponseHeader.create, req$prime)))(function (v) {
      var v1 = Control_Monad_Except.runExcept(fromResponse$prime(v.body));

      if (v1 instanceof Data_Either.Left) {
        return Control_Applicative.pure(Effect_Aff.applicativeAff)({
          body: new Data_Either.Left(new Affjax_ResponseFormat.ResponseFormatError(Data_List_NonEmpty.head(v1.value0), v.body)),
          headers: v.headers,
          status: v.status,
          statusText: v.statusText
        });
      }

      ;

      if (v1 instanceof Data_Either.Right) {
        return Control_Applicative.pure(Effect_Aff.applicativeAff)({
          body: new Data_Either.Right(v1.value0),
          headers: v.headers,
          status: v.status,
          statusText: v.statusText
        });
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 248, column 3 - line 252, column 39): " + [v1.constructor.name]);
    });
  };

  exports["request"] = request;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Reader.Class"] = $PS["Control.Monad.Reader.Class"] || {};
  var exports = $PS["Control.Monad.Reader.Class"];
  var Data_Functor = $PS["Data.Functor"];

  var MonadAsk = function MonadAsk(Monad0, ask) {
    this.Monad0 = Monad0;
    this.ask = ask;
  };

  var ask = function ask(dict) {
    return dict.ask;
  };

  var asks = function asks(dictMonadAsk) {
    return function (f) {
      return Data_Functor.map(dictMonadAsk.Monad0().Bind1().Apply0().Functor0())(f)(ask(dictMonadAsk));
    };
  };

  exports["ask"] = ask;
  exports["MonadAsk"] = MonadAsk;
  exports["asks"] = asks;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Exists"] = $PS["Data.Exists"] || {};
  var exports = $PS["Data.Exists"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var runExists = Unsafe_Coerce.unsafeCoerce;
  var mkExists = Unsafe_Coerce.unsafeCoerce;
  exports["mkExists"] = mkExists;
  exports["runExists"] = runExists;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Coyoneda"] = $PS["Data.Coyoneda"] || {};
  var exports = $PS["Data.Coyoneda"];
  var Control_Category = $PS["Control.Category"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Functor = $PS["Data.Functor"];

  var CoyonedaF = function () {
    function CoyonedaF(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CoyonedaF.create = function (value0) {
      return function (value1) {
        return new CoyonedaF(value0, value1);
      };
    };

    return CoyonedaF;
  }();

  var Coyoneda = function Coyoneda(x) {
    return x;
  };

  var unCoyoneda = function unCoyoneda(f) {
    return function (v) {
      return Data_Exists.runExists(function (v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };

  var coyoneda = function coyoneda(k) {
    return function (fi) {
      return Coyoneda(Data_Exists.mkExists(new CoyonedaF(k, fi)));
    };
  };

  var functorCoyoneda = new Data_Functor.Functor(function (f) {
    return function (v) {
      return Data_Exists.runExists(function (v1) {
        return coyoneda(function ($85) {
          return f(v1.value0($85));
        })(v1.value1);
      })(v);
    };
  });
  var liftCoyoneda = coyoneda(Control_Category.identity(Control_Category.categoryFn));
  exports["unCoyoneda"] = unCoyoneda;
  exports["liftCoyoneda"] = liftCoyoneda;
  exports["functorCoyoneda"] = functorCoyoneda;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Map.Internal"] = $PS["Data.Map.Internal"] || {};
  var exports = $PS["Data.Map.Internal"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Leaf = function () {
    function Leaf() {}

    ;
    Leaf.value = new Leaf();
    return Leaf;
  }();

  var Two = function () {
    function Two(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Two.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Two(value0, value1, value2, value3);
          };
        };
      };
    };

    return Two;
  }();

  var Three = function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }

    ;

    Three.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return function (value6) {
                  return new Three(value0, value1, value2, value3, value4, value5, value6);
                };
              };
            };
          };
        };
      };
    };

    return Three;
  }();

  var TwoLeft = function () {
    function TwoLeft(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    TwoLeft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new TwoLeft(value0, value1, value2);
        };
      };
    };

    return TwoLeft;
  }();

  var TwoRight = function () {
    function TwoRight(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    TwoRight.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new TwoRight(value0, value1, value2);
        };
      };
    };

    return TwoRight;
  }();

  var ThreeLeft = function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeLeft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeLeft(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeLeft;
  }();

  var ThreeMiddle = function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeMiddle.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeMiddle;
  }();

  var ThreeRight = function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeRight.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeRight(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeRight;
  }();

  var KickUp = function () {
    function KickUp(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    KickUp.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new KickUp(value0, value1, value2, value3);
          };
        };
      };
    };

    return KickUp;
  }();

  var values = function values(v) {
    if (v instanceof Leaf) {
      return Data_List_Types.Nil.value;
    }

    ;

    if (v instanceof Two) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(values(v.value3)));
    }

    ;

    if (v instanceof Three) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value5))(values(v.value6)))));
    }

    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 612, column 1 - line 612, column 40): " + [v.constructor.name]);
  };

  var lookup = function lookup(dictOrd) {
    return function (k) {
      var comp = Data_Ord.compare(dictOrd);

      var go = function go($copy_v) {
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Data_Maybe.Nothing.value;
          }

          ;

          if (v instanceof Two) {
            var v2 = comp(k)(v.value1);

            if (v2 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value2);
            }

            ;

            if (v2 instanceof Data_Ordering.LT) {
              $copy_v = v.value0;
              return;
            }

            ;
            $copy_v = v.value3;
            return;
          }

          ;

          if (v instanceof Three) {
            var v3 = comp(k)(v.value1);

            if (v3 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value2);
            }

            ;
            var v4 = comp(k)(v.value4);

            if (v4 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value5);
            }

            ;

            if (v3 instanceof Data_Ordering.LT) {
              $copy_v = v.value0;
              return;
            }

            ;

            if (v4 instanceof Data_Ordering.GT) {
              $copy_v = v.value6;
              return;
            }

            ;
            $copy_v = v.value3;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 200, column 5 - line 200, column 22): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }

        ;
        return $tco_result;
      };

      return go;
    };
  };

  var member = function member(dictOrd) {
    return function (k) {
      return function (m) {
        return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
      };
    };
  };

  var functorMap = new Data_Functor.Functor(function (v) {
    return function (v1) {
      if (v1 instanceof Leaf) {
        return Leaf.value;
      }

      ;

      if (v1 instanceof Two) {
        return new Two(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3));
      }

      ;

      if (v1 instanceof Three) {
        return new Three(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Data_Functor.map(functorMap)(v)(v1.value6));
      }

      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 96, column 1 - line 99, column 110): " + [v.constructor.name, v1.constructor.name]);
    };
  });

  var fromZipper = function fromZipper($copy_dictOrd) {
    return function ($copy_v) {
      return function ($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return tree;
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }

            ;

            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }

            ;

            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }

            ;

            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }

            ;

            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 418, column 3 - line 423, column 88): " + [v.value0.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 415, column 1 - line 415, column 80): " + [v.constructor.name, tree.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }

        ;
        return $tco_result;
      };
    };
  };

  var insert = function insert(dictOrd) {
    return function (k) {
      return function (v) {
        var up = function up($copy_v1) {
          return function ($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(v1, v2) {
              if (v1 instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }

              ;

              if (v1 instanceof Data_List_Types.Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }

                ;

                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }

                ;

                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }

                ;

                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }

                ;

                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }

                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 454, column 5 - line 459, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 451, column 3 - line 451, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }

            ;
            return $tco_result;
          };
        };

        var comp = Data_Ord.compare(dictOrd);

        var down = function down($copy_ctx) {
          return function ($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }

              ;

              if (v1 instanceof Two) {
                var v2 = comp(k)(v1.value1);

                if (v2 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                }

                ;

                if (v2 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }

                ;
                $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }

              ;

              if (v1 instanceof Three) {
                var v3 = comp(k)(v1.value1);

                if (v3 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }

                ;
                var v4 = comp(k)(v1.value4);

                if (v4 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }

                ;

                if (v3 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }

                ;

                if (v3 instanceof Data_Ordering.GT && v4 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }

                ;
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 434, column 3 - line 434, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }

            ;
            return $tco_result;
          };
        };

        return down(Data_List_Types.Nil.value);
      };
    };
  };

  var pop = function pop(dictOrd) {
    return function (k) {
      var up = function up($copy_ctxs) {
        return function ($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return tree;
            }

            ;

            if (ctxs instanceof Data_List_Types.Cons) {
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }

              ;

              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Leaf && ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Leaf && ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value0 instanceof Leaf && ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 511, column 9 - line 528, column 136): " + [ctxs.value0.constructor.name, tree.constructor.name]);
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 508, column 5 - line 528, column 136): " + [ctxs.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }

          ;
          return $tco_result;
        };
      };

      var removeMaxNode = function removeMaxNode($copy_ctx) {
        return function ($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctx, m) {
            if (m instanceof Two && m.value0 instanceof Leaf && m.value3 instanceof Leaf) {
              $tco_done = true;
              return up(ctx)(Leaf.value);
            }

            ;

            if (m instanceof Two) {
              $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }

            ;

            if (m instanceof Three && m.value0 instanceof Leaf && m.value3 instanceof Leaf && m.value6 instanceof Leaf) {
              $tco_done = true;
              return up(new Data_List_Types.Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }

            ;

            if (m instanceof Three) {
              $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 540, column 5 - line 544, column 107): " + [m.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }

          ;
          return $tco_result;
        };
      };

      var maxNode = function maxNode($copy_m) {
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }

          ;

          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }

          ;

          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }

          ;

          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 531, column 33 - line 535, column 45): " + [m.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($copy_m);
        }

        ;
        return $tco_result;
      };

      var comp = Data_Ord.compare(dictOrd);

      var down = function down($copy_ctx) {
        return function ($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done = true;
              return Data_Maybe.Nothing.value;
            }

            ;

            if (m instanceof Two) {
              var v = comp(k)(m.value1);

              if (m.value3 instanceof Leaf && v instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, up(ctx)(Leaf.value)));
              }

              ;

              if (v instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value0);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new TwoLeft(max.key, max.value, m.value3), ctx))(m.value0)));
              }

              ;

              if (v instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }

              ;
              $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }

            ;

            if (m instanceof Three) {
              var leaves = function () {
                if (m.value0 instanceof Leaf && m.value3 instanceof Leaf && m.value6 instanceof Leaf) {
                  return true;
                }

                ;
                return false;
              }();

              var v = comp(k)(m.value4);
              var v3 = comp(k)(m.value1);

              if (leaves && v3 instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }

              ;

              if (leaves && v instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }

              ;

              if (v3 instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value0);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new ThreeLeft(max.key, max.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }

              ;

              if (v instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value3);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, removeMaxNode(new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max.key, max.value, m.value6), ctx))(m.value3)));
              }

              ;

              if (v3 instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }

              ;

              if (v3 instanceof Data_Ordering.GT && v instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }

              ;
              $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 481, column 34 - line 504, column 80): " + [m.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }

          ;
          return $tco_result;
        };
      };

      return down(Data_List_Types.Nil.value);
    };
  };

  var foldableMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (m) {
        return Data_Foldable.foldMap(Data_List_Types.foldableList)(dictMonoid)(f)(values(m));
      };
    };
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldl(Data_List_Types.foldableList)(f)(z)(values(m));
      };
    };
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_List_Types.foldableList)(f)(z)(values(m));
      };
    };
  });
  var empty = Leaf.value;

  var $$delete = function $$delete(dictOrd) {
    return function (k) {
      return function (m) {
        return Data_Maybe.maybe(m)(Data_Tuple.snd)(pop(dictOrd)(k)(m));
      };
    };
  };

  var alter = function alter(dictOrd) {
    return function (f) {
      return function (k) {
        return function (m) {
          var v = f(lookup(dictOrd)(k)(m));

          if (v instanceof Data_Maybe.Nothing) {
            return $$delete(dictOrd)(k)(m);
          }

          ;

          if (v instanceof Data_Maybe.Just) {
            return insert(dictOrd)(k)(v.value0)(m);
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 549, column 15 - line 551, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  exports["empty"] = empty;
  exports["insert"] = insert;
  exports["lookup"] = lookup;
  exports["delete"] = $$delete;
  exports["pop"] = pop;
  exports["member"] = member;
  exports["alter"] = alter;
  exports["functorMap"] = functorMap;
  exports["foldableMap"] = foldableMap;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Data.OrdBox"] = $PS["Halogen.Data.OrdBox"] || {};
  var exports = $PS["Halogen.Data.OrdBox"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ord = $PS["Data.Ord"];

  var OrdBox = function () {
    function OrdBox(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    OrdBox.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new OrdBox(value0, value1, value2);
        };
      };
    };

    return OrdBox;
  }();

  var mkOrdBox = function mkOrdBox(dictOrd) {
    return OrdBox.create(Data_Eq.eq(dictOrd.Eq0()))(Data_Ord.compare(dictOrd));
  };

  var eqOrdBox = new Data_Eq.Eq(function (v) {
    return function (v1) {
      return v.value0(v.value2)(v1.value2);
    };
  });
  var ordOrdBox = new Data_Ord.Ord(function () {
    return eqOrdBox;
  }, function (v) {
    return function (v1) {
      return v.value1(v.value2)(v1.value2);
    };
  });
  exports["mkOrdBox"] = mkOrdBox;
  exports["ordOrdBox"] = ordOrdBox;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Data.Slot"] = $PS["Halogen.Data.Slot"] || {};
  var exports = $PS["Halogen.Data.Slot"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Halogen_Data_OrdBox = $PS["Halogen.Data.OrdBox"];

  var pop = function pop(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (v) {
              return Data_Map_Internal.pop(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };

  var lookup = function lookup(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (v) {
              return Data_Map_Internal.lookup(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };

  var insert = function insert(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (val) {
              return function (v) {
                return Data_Map_Internal.insert(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };

  var foreachSlot = function foreachSlot(dictApplicative) {
    return function (v) {
      return function (k) {
        return Data_Foldable.traverse_(dictApplicative)(Data_Map_Internal.foldableMap)(function ($37) {
          return k($37);
        })(v);
      };
    };
  };

  var empty = Data_Map_Internal.empty;
  exports["empty"] = empty;
  exports["lookup"] = lookup;
  exports["insert"] = insert;
  exports["pop"] = pop;
  exports["foreachSlot"] = foreachSlot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Applicative.Free"] = $PS["Control.Applicative.Free"] || {};
  var exports = $PS["Control.Applicative.Free"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Pure = function () {
    function Pure(value0) {
      this.value0 = value0;
    }

    ;

    Pure.create = function (value0) {
      return new Pure(value0);
    };

    return Pure;
  }();

  var Lift = function () {
    function Lift(value0) {
      this.value0 = value0;
    }

    ;

    Lift.create = function (value0) {
      return new Lift(value0);
    };

    return Lift;
  }();

  var Ap = function () {
    function Ap(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Ap.create = function (value0) {
      return function (value1) {
        return new Ap(value0, value1);
      };
    };

    return Ap;
  }();

  var mkAp = function mkAp(fba) {
    return function (fb) {
      return new Ap(fba, fb);
    };
  };

  var liftFreeAp = Lift.create;

  var goLeft = function goLeft($copy_dictApplicative) {
    return function ($copy_fStack) {
      return function ($copy_valStack) {
        return function ($copy_nat) {
          return function ($copy_func) {
            return function ($copy_count) {
              var $tco_var_dictApplicative = $copy_dictApplicative;
              var $tco_var_fStack = $copy_fStack;
              var $tco_var_valStack = $copy_valStack;
              var $tco_var_nat = $copy_nat;
              var $tco_var_func = $copy_func;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(dictApplicative, fStack, valStack, nat, func, count) {
                if (func instanceof Pure) {
                  $tco_done = true;
                  return new Data_Tuple.Tuple(new Data_List_Types.Cons({
                    func: Control_Applicative.pure(dictApplicative)(func.value0),
                    count: count
                  }, fStack), valStack);
                }

                ;

                if (func instanceof Lift) {
                  $tco_done = true;
                  return new Data_Tuple.Tuple(new Data_List_Types.Cons({
                    func: nat(func.value0),
                    count: count
                  }, fStack), valStack);
                }

                ;

                if (func instanceof Ap) {
                  $tco_var_dictApplicative = dictApplicative;
                  $tco_var_fStack = fStack;
                  $tco_var_valStack = Data_List_NonEmpty.cons(func.value1)(valStack);
                  $tco_var_nat = nat;
                  $tco_var_func = func.value0;
                  $copy_count = count + 1 | 0;
                  return;
                }

                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_valStack, $tco_var_nat, $tco_var_func, $copy_count);
              }

              ;
              return $tco_result;
            };
          };
        };
      };
    };
  };

  var goApply = function goApply($copy_dictApplicative) {
    return function ($copy_fStack) {
      return function ($copy_vals) {
        return function ($copy_gVal) {
          var $tco_var_dictApplicative = $copy_dictApplicative;
          var $tco_var_fStack = $copy_fStack;
          var $tco_var_vals = $copy_vals;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(dictApplicative, fStack, vals, gVal) {
            if (fStack instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return new Data_Either.Left(gVal);
            }

            ;

            if (fStack instanceof Data_List_Types.Cons) {
              var gRes = Control_Apply.apply(dictApplicative.Apply0())(fStack.value0.func)(gVal);
              var $14 = fStack.value0.count === 1;

              if ($14) {
                if (fStack.value1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return new Data_Either.Left(gRes);
                }

                ;
                $tco_var_dictApplicative = dictApplicative;
                $tco_var_fStack = fStack.value1;
                $tco_var_vals = vals;
                $copy_gVal = gRes;
                return;
              }

              ;

              if (vals instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return new Data_Either.Left(gRes);
              }

              ;

              if (vals instanceof Data_List_Types.Cons) {
                $tco_done = true;
                return Data_Either.Right.create(new Data_Tuple.Tuple(new Data_List_Types.Cons({
                  func: gRes,
                  count: fStack.value0.count - 1 | 0
                }, fStack.value1), new Data_NonEmpty.NonEmpty(vals.value0, vals.value1)));
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
            }

            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_vals, $copy_gVal);
          }

          ;
          return $tco_result;
        };
      };
    };
  };

  var functorFreeAp = new Data_Functor.Functor(function (f) {
    return function (x) {
      return mkAp(new Pure(f))(x);
    };
  });

  var foldFreeAp = function foldFreeAp(dictApplicative) {
    return function (nat) {
      return function (z) {
        var go = function go($copy_v) {
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(Control_Applicative.pure(dictApplicative)(v.value1.value0.value0));

              if (v1 instanceof Data_Either.Left) {
                $tco_done = true;
                return v1.value0;
              }

              ;

              if (v1 instanceof Data_Either.Right) {
                $copy_v = v1.value0;
                return;
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }

            ;

            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));

              if (v1 instanceof Data_Either.Left) {
                $tco_done = true;
                return v1.value0;
              }

              ;

              if (v1 instanceof Data_Either.Right) {
                $copy_v = v1.value0;
                return;
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }

            ;

            if (v.value1.value0 instanceof Ap) {
              var nextVals = new Data_NonEmpty.NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft(dictApplicative)(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }

            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }

          ;
          return $tco_result;
        };

        return go(new Data_Tuple.Tuple(Data_List_Types.Nil.value, Data_List_NonEmpty.singleton(z)));
      };
    };
  };

  var retractFreeAp = function retractFreeAp(dictApplicative) {
    return foldFreeAp(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  };

  var applyFreeAp = new Control_Apply.Apply(function () {
    return functorFreeAp;
  }, function (fba) {
    return function (fb) {
      return mkAp(fba)(fb);
    };
  });
  var applicativeFreeAp = new Control_Applicative.Applicative(function () {
    return applyFreeAp;
  }, Pure.create);

  var hoistFreeAp = function hoistFreeAp(f) {
    return foldFreeAp(applicativeFreeAp)(function ($37) {
      return liftFreeAp(f($37));
    });
  };

  exports["retractFreeAp"] = retractFreeAp;
  exports["hoistFreeAp"] = hoistFreeAp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.List"] = $PS["Data.List"] || {};
  var exports = $PS["Data.List"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];

  var reverse = function () {
    var go = function go($copy_acc) {
      return function ($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(acc, v) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return acc;
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go(Data_List_Types.Nil.value);
  }();

  var $$null = function $$null(v) {
    if (v instanceof Data_List_Types.Nil) {
      return true;
    }

    ;
    return false;
  };

  var some = function some(dictAlternative) {
    return function (dictLazy) {
      return function (v) {
        return Control_Apply.apply(dictAlternative.Applicative0().Apply0())(Data_Functor.map(dictAlternative.Plus1().Alt0().Functor0())(Data_List_Types.Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };

  var many = function many(dictAlternative) {
    return function (dictLazy) {
      return function (v) {
        return Control_Alt.alt(dictAlternative.Plus1().Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())(Data_List_Types.Nil.value));
      };
    };
  };

  exports["some"] = some;
  exports["null"] = $$null;
  exports["reverse"] = reverse;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.CatQueue"] = $PS["Data.CatQueue"] || {};
  var exports = $PS["Data.CatQueue"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];

  var CatQueue = function () {
    function CatQueue(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CatQueue.create = function (value0) {
      return function (value1) {
        return new CatQueue(value0, value1);
      };
    };

    return CatQueue;
  }();

  var uncons = function uncons($copy_v) {
    var $tco_done = false;
    var $tco_result;

    function $tco_loop(v) {
      if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
        $tco_done = true;
        return Data_Maybe.Nothing.value;
      }

      ;

      if (v.value0 instanceof Data_List_Types.Nil) {
        $copy_v = new CatQueue(Data_List.reverse(v.value1), Data_List_Types.Nil.value);
        return;
      }

      ;

      if (v.value0 instanceof Data_List_Types.Cons) {
        $tco_done = true;
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }

      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 83, column 1 - line 83, column 63): " + [v.constructor.name]);
    }

    ;

    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  var snoc = function snoc(v) {
    return function (a) {
      return new CatQueue(v.value0, new Data_List_Types.Cons(a, v.value1));
    };
  };

  var $$null = function $$null(v) {
    if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
      return true;
    }

    ;
    return false;
  };

  var empty = new CatQueue(Data_List_Types.Nil.value, Data_List_Types.Nil.value);
  exports["empty"] = empty;
  exports["null"] = $$null;
  exports["snoc"] = snoc;
  exports["uncons"] = uncons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.CatList"] = $PS["Data.CatList"] || {};
  var exports = $PS["Data.CatList"];
  var Data_CatQueue = $PS["Data.CatQueue"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Tuple = $PS["Data.Tuple"];

  var CatNil = function () {
    function CatNil() {}

    ;
    CatNil.value = new CatNil();
    return CatNil;
  }();

  var CatCons = function () {
    function CatCons(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CatCons.create = function (value0) {
      return function (value1) {
        return new CatCons(value0, value1);
      };
    };

    return CatCons;
  }();

  var link = function link(v) {
    return function (v1) {
      if (v instanceof CatNil) {
        return v1;
      }

      ;

      if (v1 instanceof CatNil) {
        return v;
      }

      ;

      if (v instanceof CatCons) {
        return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(v1));
      }

      ;
      throw new Error("Failed pattern match at Data.CatList (line 109, column 1 - line 109, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };

  var foldr = function foldr(k) {
    return function (b) {
      return function (q) {
        var foldl = function foldl($copy_v) {
          return function ($copy_c) {
            return function ($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_var_c = $copy_c;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(v, c, v1) {
                if (v1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return c;
                }

                ;

                if (v1 instanceof Data_List_Types.Cons) {
                  $tco_var_v = v;
                  $tco_var_c = v(c)(v1.value0);
                  $copy_v1 = v1.value1;
                  return;
                }

                ;
                throw new Error("Failed pattern match at Data.CatList (line 125, column 3 - line 125, column 59): " + [v.constructor.name, c.constructor.name, v1.constructor.name]);
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
              }

              ;
              return $tco_result;
            };
          };
        };

        var go = function go($copy_xs) {
          return function ($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(xs, ys) {
              var v = Data_CatQueue.uncons(xs);

              if (v instanceof Data_Maybe.Nothing) {
                $tco_done = true;
                return foldl(function (x) {
                  return function (i) {
                    return i(x);
                  };
                })(b)(ys);
              }

              ;

              if (v instanceof Data_Maybe.Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Data_List_Types.Cons(k(v.value0.value0), ys);
                return;
              }

              ;
              throw new Error("Failed pattern match at Data.CatList (line 121, column 14 - line 123, column 67): " + [v.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }

            ;
            return $tco_result;
          };
        };

        return go(q)(Data_List_Types.Nil.value);
      };
    };
  };

  var uncons = function uncons(v) {
    if (v instanceof CatNil) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v instanceof CatCons) {
      return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, function () {
        var $44 = Data_CatQueue["null"](v.value1);

        if ($44) {
          return CatNil.value;
        }

        ;
        return foldr(link)(CatNil.value)(v.value1);
      }()));
    }

    ;
    throw new Error("Failed pattern match at Data.CatList (line 100, column 1 - line 100, column 61): " + [v.constructor.name]);
  };

  var empty = CatNil.value;
  var append = link;
  var semigroupCatList = new Data_Semigroup.Semigroup(append);

  var snoc = function snoc(cat) {
    return function (a) {
      return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
  };

  exports["empty"] = empty;
  exports["snoc"] = snoc;
  exports["uncons"] = uncons;
  exports["semigroupCatList"] = semigroupCatList;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Free"] = $PS["Control.Monad.Free"] || {};
  var exports = $PS["Control.Monad.Free"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Data_CatList = $PS["Data.CatList"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Free = function () {
    function Free(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Free.create = function (value0) {
      return function (value1) {
        return new Free(value0, value1);
      };
    };

    return Free;
  }();

  var Return = function () {
    function Return(value0) {
      this.value0 = value0;
    }

    ;

    Return.create = function (value0) {
      return new Return(value0);
    };

    return Return;
  }();

  var Bind = function () {
    function Bind(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Bind.create = function (value0) {
      return function (value1) {
        return new Bind(value0, value1);
      };
    };

    return Bind;
  }();

  var toView = function toView($copy_v) {
    var $tco_done = false;
    var $tco_result;

    function $tco_loop(v) {
      var runExpF = function runExpF(v2) {
        return v2;
      };

      var concatF = function concatF(v2) {
        return function (r) {
          return new Free(v2.value0, Data_Semigroup.append(Data_CatList.semigroupCatList)(v2.value1)(r));
        };
      };

      if (v.value0 instanceof Return) {
        var v2 = Data_CatList.uncons(v.value1);

        if (v2 instanceof Data_Maybe.Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }

        ;

        if (v2 instanceof Data_Maybe.Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }

      ;

      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function (a) {
          return concatF(v.value0.value1(a))(v.value1);
        });
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }

    ;

    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  var fromView = function fromView(f) {
    return new Free(f, Data_CatList.empty);
  };

  var freeMonad = new Control_Monad.Monad(function () {
    return freeApplicative;
  }, function () {
    return freeBind;
  });
  var freeFunctor = new Data_Functor.Functor(function (k) {
    return function (f) {
      return Control_Bind.bindFlipped(freeBind)(function () {
        var $120 = Control_Applicative.pure(freeApplicative);
        return function ($121) {
          return $120(k($121));
        };
      }())(f);
    };
  });
  var freeBind = new Control_Bind.Bind(function () {
    return freeApply;
  }, function (v) {
    return function (k) {
      return new Free(v.value0, Data_CatList.snoc(v.value1)(k));
    };
  });
  var freeApply = new Control_Apply.Apply(function () {
    return freeFunctor;
  }, Control_Monad.ap(freeMonad));
  var freeApplicative = new Control_Applicative.Applicative(function () {
    return freeApply;
  }, function ($122) {
    return fromView(Return.create($122));
  });

  var liftF = function liftF(f) {
    return fromView(new Bind(f, function () {
      var $123 = Control_Applicative.pure(freeApplicative);
      return function ($124) {
        return $123($124);
      };
    }()));
  };

  var substFree = function substFree(k) {
    var go = function go(f) {
      var v = toView(f);

      if (v instanceof Return) {
        return Control_Applicative.pure(freeApplicative)(v.value0);
      }

      ;

      if (v instanceof Bind) {
        return Control_Bind.bind(freeBind)(k(v.value0))(Data_Functor.map(Data_Functor.functorFn)(go)(v.value1));
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 168, column 10 - line 170, column 33): " + [v.constructor.name]);
    };

    return go;
  };

  var hoistFree = function hoistFree(k) {
    return substFree(function ($125) {
      return liftF(k($125));
    });
  };

  var foldFree = function foldFree(dictMonadRec) {
    return function (k) {
      var go = function go(f) {
        var v = toView(f);

        if (v instanceof Return) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Done.create)(Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(v.value0));
        }

        ;

        if (v instanceof Bind) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(function ($136) {
            return Control_Monad_Rec_Class.Loop.create(v.value1($136));
          })(k(v.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };

      return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
  };

  exports["liftF"] = liftF;
  exports["hoistFree"] = hoistFree;
  exports["foldFree"] = foldFree;
  exports["freeFunctor"] = freeFunctor;
  exports["freeBind"] = freeBind;
  exports["freeApplicative"] = freeApplicative;
  exports["freeApply"] = freeApply;
  exports["freeMonad"] = freeMonad;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Reader.Trans"] = $PS["Control.Monad.Reader.Trans"] || {};
  var exports = $PS["Control.Monad.Reader.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect_Class = $PS["Effect.Class"];

  var ReaderT = function ReaderT(x) {
    return x;
  };

  var runReaderT = function runReaderT(v) {
    return v;
  };

  var monadTransReaderT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function ($67) {
      return ReaderT(Data_Function["const"]($67));
    };
  });

  var mapReaderT = function mapReaderT(f) {
    return function (v) {
      return function ($68) {
        return f(v($68));
      };
    };
  };

  var functorReaderT = function functorReaderT(dictFunctor) {
    return new Data_Functor.Functor(function () {
      var $69 = Data_Functor.map(dictFunctor);
      return function ($70) {
        return mapReaderT($69($70));
      };
    }());
  };

  var applyReaderT = function applyReaderT(dictApply) {
    return new Control_Apply.Apply(function () {
      return functorReaderT(dictApply.Functor0());
    }, function (v) {
      return function (v1) {
        return function (r) {
          return Control_Apply.apply(dictApply)(v(r))(v1(r));
        };
      };
    });
  };

  var bindReaderT = function bindReaderT(dictBind) {
    return new Control_Bind.Bind(function () {
      return applyReaderT(dictBind.Apply0());
    }, function (v) {
      return function (k) {
        return function (r) {
          return Control_Bind.bind(dictBind)(v(r))(function (a) {
            var v1 = k(a);
            return v1(r);
          });
        };
      };
    });
  };

  var applicativeReaderT = function applicativeReaderT(dictApplicative) {
    return new Control_Applicative.Applicative(function () {
      return applyReaderT(dictApplicative.Apply0());
    }, function () {
      var $74 = Control_Applicative.pure(dictApplicative);
      return function ($75) {
        return ReaderT(Data_Function["const"]($74($75)));
      };
    }());
  };

  var monadReaderT = function monadReaderT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeReaderT(dictMonad.Applicative0());
    }, function () {
      return bindReaderT(dictMonad.Bind1());
    });
  };

  var monadAskReaderT = function monadAskReaderT(dictMonad) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
      return monadReaderT(dictMonad);
    }, Control_Applicative.pure(dictMonad.Applicative0()));
  };

  var monadEffectReader = function monadEffectReader(dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
      return monadReaderT(dictMonadEffect.Monad0());
    }, function () {
      var $77 = Control_Monad_Trans_Class.lift(monadTransReaderT)(dictMonadEffect.Monad0());
      var $78 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($79) {
        return $77($78($79));
      };
    }());
  };

  exports["runReaderT"] = runReaderT;
  exports["functorReaderT"] = functorReaderT;
  exports["bindReaderT"] = bindReaderT;
  exports["monadReaderT"] = monadReaderT;
  exports["monadTransReaderT"] = monadTransReaderT;
  exports["monadEffectReader"] = monadEffectReader;
  exports["monadAskReaderT"] = monadAskReaderT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Aff.Class"] = $PS["Effect.Aff.Class"] || {};
  var exports = $PS["Effect.Aff.Class"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Reader_Trans = $PS["Control.Monad.Reader.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Effect_Aff = $PS["Effect.Aff"];

  var MonadAff = function MonadAff(MonadEffect0, liftAff) {
    this.MonadEffect0 = MonadEffect0;
    this.liftAff = liftAff;
  };

  var monadAffAff = new MonadAff(function () {
    return Effect_Aff.monadEffectAff;
  }, Control_Category.identity(Control_Category.categoryFn));

  var liftAff = function liftAff(dict) {
    return dict.liftAff;
  };

  var monadAffReader = function monadAffReader(dictMonadAff) {
    return new MonadAff(function () {
      return Control_Monad_Reader_Trans.monadEffectReader(dictMonadAff.MonadEffect0());
    }, function () {
      var $25 = Control_Monad_Trans_Class.lift(Control_Monad_Reader_Trans.monadTransReaderT)(dictMonadAff.MonadEffect0().Monad0());
      var $26 = liftAff(dictMonadAff);
      return function ($27) {
        return $25($26($27));
      };
    }());
  };

  exports["liftAff"] = liftAff;
  exports["MonadAff"] = MonadAff;
  exports["monadAffAff"] = monadAffAff;
  exports["monadAffReader"] = monadAffReader;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query.ChildQuery"] = $PS["Halogen.Query.ChildQuery"] || {};
  var exports = $PS["Halogen.Query.ChildQuery"];
  var Data_Functor = $PS["Data.Functor"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var ChildQuery = function () {
    function ChildQuery(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    ChildQuery.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new ChildQuery(value0, value1, value2);
        };
      };
    };

    return ChildQuery;
  }();

  var unChildQueryBox = Unsafe_Coerce.unsafeCoerce;
  var mkChildQueryBox = Unsafe_Coerce.unsafeCoerce;
  var functorChildQuery = new Data_Functor.Functor(function (f) {
    return unChildQueryBox(function (v) {
      return mkChildQueryBox(new ChildQuery(function (dictApplicative) {
        return v.value0(dictApplicative);
      }, v.value1, function ($6) {
        return f(v.value2($6));
      }));
    });
  });
  exports["ChildQuery"] = ChildQuery;
  exports["mkChildQueryBox"] = mkChildQueryBox;
  exports["unChildQueryBox"] = unChildQueryBox;
  exports["functorChildQuery"] = functorChildQuery;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Free.Trans"] = $PS["Control.Monad.Free.Trans"] || {};
  var exports = $PS["Control.Monad.Free.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];

  var Bound = function () {
    function Bound(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Bound.create = function (value0) {
      return function (value1) {
        return new Bound(value0, value1);
      };
    };

    return Bound;
  }();

  var FreeT = function () {
    function FreeT(value0) {
      this.value0 = value0;
    }

    ;

    FreeT.create = function (value0) {
      return new FreeT(value0);
    };

    return FreeT;
  }();

  var Bind = function () {
    function Bind(value0) {
      this.value0 = value0;
    }

    ;

    Bind.create = function (value0) {
      return new Bind(value0);
    };

    return Bind;
  }();

  var monadTransFreeT = function monadTransFreeT(dictFunctor) {
    return new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
      return function (ma) {
        return new FreeT(function (v) {
          return Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(Data_Either.Left.create)(ma);
        });
      };
    });
  };

  var freeT = FreeT.create;

  var bound = function bound(m) {
    return function (f) {
      return new Bind(Data_Exists.mkExists(new Bound(m, f)));
    };
  };

  var functorFreeT = function functorFreeT(dictFunctor) {
    return function (dictFunctor1) {
      return new Data_Functor.Functor(function (f) {
        return function (v) {
          if (v instanceof FreeT) {
            return new FreeT(function (v1) {
              return Data_Functor.map(dictFunctor1)(Data_Bifunctor.bimap(Data_Either.bifunctorEither)(f)(Data_Functor.map(dictFunctor)(Data_Functor.map(functorFreeT(dictFunctor)(dictFunctor1))(f))))(v.value0(Data_Unit.unit));
            });
          }

          ;

          if (v instanceof Bind) {
            return Data_Exists.runExists(function (v1) {
              return bound(v1.value0)(function () {
                var $104 = Data_Functor.map(functorFreeT(dictFunctor)(dictFunctor1))(f);
                return function ($105) {
                  return $104(v1.value1($105));
                };
              }());
            })(v.value0);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 57, column 1 - line 59, column 71): " + [f.constructor.name, v.constructor.name]);
        };
      });
    };
  };

  var bimapFreeT = function bimapFreeT(dictFunctor) {
    return function (dictFunctor1) {
      return function (nf) {
        return function (nm) {
          return function (v) {
            if (v instanceof Bind) {
              return Data_Exists.runExists(function (v1) {
                return bound(function () {
                  var $106 = bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm);
                  return function ($107) {
                    return $106(v1.value0($107));
                  };
                }())(function () {
                  var $108 = bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm);
                  return function ($109) {
                    return $108(v1.value1($109));
                  };
                }());
              })(v.value0);
            }

            ;

            if (v instanceof FreeT) {
              return new FreeT(function (v1) {
                return Data_Functor.map(dictFunctor1)(Data_Functor.map(Data_Either.functorEither)(function () {
                  var $110 = Data_Functor.map(dictFunctor)(bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm));
                  return function ($111) {
                    return nf($110($111));
                  };
                }()))(nm(v.value0(Data_Unit.unit)));
              });
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 118, column 1 - line 118, column 109): " + [nf.constructor.name, nm.constructor.name, v.constructor.name]);
          };
        };
      };
    };
  };

  var hoistFreeT = function hoistFreeT(dictFunctor) {
    return function (dictFunctor1) {
      return bimapFreeT(dictFunctor)(dictFunctor1)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var interpret = function interpret(dictFunctor) {
    return function (dictFunctor1) {
      return function (nf) {
        return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(Control_Category.identity(Control_Category.categoryFn));
      };
    };
  };

  var monadFreeT = function monadFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Monad.Monad(function () {
        return applicativeFreeT(dictFunctor)(dictMonad);
      }, function () {
        return bindFreeT(dictFunctor)(dictMonad);
      });
    };
  };

  var bindFreeT = function bindFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Bind.Bind(function () {
        return applyFreeT(dictFunctor)(dictMonad);
      }, function (v) {
        return function (f) {
          if (v instanceof Bind) {
            return Data_Exists.runExists(function (v1) {
              return bound(v1.value0)(function (x) {
                return bound(function (v2) {
                  return v1.value1(x);
                })(f);
              });
            })(v.value0);
          }

          ;
          return bound(function (v1) {
            return v;
          })(f);
        };
      });
    };
  };

  var applyFreeT = function applyFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Apply.Apply(function () {
        return functorFreeT(dictFunctor)(dictMonad.Bind1().Apply0().Functor0());
      }, Control_Monad.ap(monadFreeT(dictFunctor)(dictMonad)));
    };
  };

  var applicativeFreeT = function applicativeFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
        return applyFreeT(dictFunctor)(dictMonad);
      }, function (a) {
        return new FreeT(function (v) {
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Left(a));
        });
      });
    };
  };

  var liftFreeT = function liftFreeT(dictFunctor) {
    return function (dictMonad) {
      return function (fa) {
        return new FreeT(function (v) {
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(Data_Functor.map(dictFunctor)(Control_Applicative.pure(applicativeFreeT(dictFunctor)(dictMonad)))(fa)));
        });
      };
    };
  };

  var resume = function resume(dictFunctor) {
    return function (dictMonadRec) {
      var go = function go(v) {
        if (v instanceof FreeT) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Done.create)(v.value0(Data_Unit.unit));
        }

        ;

        if (v instanceof Bind) {
          return Data_Exists.runExists(function (v1) {
            var v2 = v1.value0(Data_Unit.unit);

            if (v2 instanceof FreeT) {
              return Control_Bind.bind(dictMonadRec.Monad0().Bind1())(v2.value0(Data_Unit.unit))(function (v3) {
                if (v3 instanceof Data_Either.Left) {
                  return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Loop(v1.value1(v3.value0)));
                }

                ;

                if (v3 instanceof Data_Either.Right) {
                  return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Done(new Data_Either.Right(Data_Functor.map(dictFunctor)(function (h) {
                    return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(h)(v1.value1);
                  })(v3.value0))));
                }

                ;
                throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 52, column 20 - line 54, column 67): " + [v3.constructor.name]);
              });
            }

            ;

            if (v2 instanceof Bind) {
              return Data_Exists.runExists(function (v3) {
                return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Loop(Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(v3.value0(Data_Unit.unit))(function (z) {
                  return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(v3.value1(z))(v1.value1);
                })));
              })(v2.value0);
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 50, column 5 - line 55, column 98): " + [v2.constructor.name]);
          })(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 47, column 3 - line 47, column 75): " + [v.constructor.name]);
      };

      return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
  };

  var runFreeT = function runFreeT(dictFunctor) {
    return function (dictMonadRec) {
      return function (interp) {
        var go = function go(v) {
          if (v instanceof Data_Either.Left) {
            return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Done(v.value0));
          }

          ;

          if (v instanceof Data_Either.Right) {
            return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Loop.create)(interp(v.value0));
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 126, column 3 - line 126, column 63): " + [v.constructor.name]);
        };

        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(Control_Bind.composeKleisliFlipped(dictMonadRec.Monad0().Bind1())(go)(resume(dictFunctor)(dictMonadRec)));
      };
    };
  };

  var monadRecFreeT = function monadRecFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadFreeT(dictFunctor)(dictMonad);
      }, function (f) {
        var go = function go(s) {
          return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonad))(f(s))(function (v) {
            if (v instanceof Control_Monad_Rec_Class.Loop) {
              return go(v.value0);
            }

            ;

            if (v instanceof Control_Monad_Rec_Class.Done) {
              return Control_Applicative.pure(applicativeFreeT(dictFunctor)(dictMonad))(v.value0);
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 80, column 15 - line 82, column 25): " + [v.constructor.name]);
          });
        };

        return go;
      });
    };
  };

  exports["freeT"] = freeT;
  exports["liftFreeT"] = liftFreeT;
  exports["hoistFreeT"] = hoistFreeT;
  exports["interpret"] = interpret;
  exports["resume"] = resume;
  exports["runFreeT"] = runFreeT;
  exports["functorFreeT"] = functorFreeT;
  exports["applicativeFreeT"] = applicativeFreeT;
  exports["bindFreeT"] = bindFreeT;
  exports["monadTransFreeT"] = monadTransFreeT;
  exports["monadRecFreeT"] = monadRecFreeT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Profunctor"] = $PS["Data.Profunctor"] || {};
  var exports = $PS["Data.Profunctor"];
  var Control_Category = $PS["Control.Category"];

  var Profunctor = function Profunctor(dimap) {
    this.dimap = dimap;
  };

  var profunctorFn = new Profunctor(function (a2b) {
    return function (c2d) {
      return function (b2c) {
        return function ($9) {
          return c2d(b2c(a2b($9)));
        };
      };
    };
  });

  var dimap = function dimap(dict) {
    return dict.dimap;
  };

  var rmap = function rmap(dictProfunctor) {
    return function (b2c) {
      return dimap(dictProfunctor)(Control_Category.identity(Control_Category.categoryFn))(b2c);
    };
  };

  exports["dimap"] = dimap;
  exports["Profunctor"] = Profunctor;
  exports["rmap"] = rmap;
  exports["profunctorFn"] = profunctorFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Coroutine"] = $PS["Control.Coroutine"] || {};
  var exports = $PS["Control.Coroutine"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_Free_Trans = $PS["Control.Monad.Free.Trans"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];

  var Emit = function () {
    function Emit(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Emit.create = function (value0) {
      return function (value1) {
        return new Emit(value0, value1);
      };
    };

    return Emit;
  }();

  var runProcess = function runProcess(dictMonadRec) {
    return Control_Monad_Free_Trans.runFreeT(Data_Identity.functorIdentity)(dictMonadRec)(function () {
      var $186 = Control_Applicative.pure(dictMonadRec.Monad0().Applicative0());
      var $187 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
      return function ($188) {
        return $186($187($188));
      };
    }());
  };

  var profunctorAwait = new Data_Profunctor.Profunctor(function (f) {
    return function (g) {
      return function (v) {
        return Data_Profunctor.dimap(Data_Profunctor.profunctorFn)(f)(g)(v);
      };
    };
  });

  var loop = function loop(dictFunctor) {
    return function (dictMonad) {
      return function (me) {
        return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(dictFunctor)(dictMonad))(function (v) {
          return Data_Functor.map(Control_Monad_Free_Trans.functorFreeT(dictFunctor)(dictMonad.Bind1().Apply0().Functor0()))(Data_Maybe.maybe(new Control_Monad_Rec_Class.Loop(Data_Unit.unit))(Control_Monad_Rec_Class.Done.create))(me);
        })(Data_Unit.unit);
      };
    };
  };

  var fuseWithL = function fuseWithL(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictFunctor2) {
        return function (dictMonadRec) {
          return function (zap) {
            return function (fs) {
              return function (gs) {
                var go = function go(v) {
                  return Control_Monad_Except_Trans.runExceptT(Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(dictMonadRec.Monad0()))(Control_Monad_Except_Trans.ExceptT(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0)))(function (v1) {
                    return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(dictMonadRec.Monad0()))(Control_Monad_Except_Trans.ExceptT(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1)))(function (v2) {
                      return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonadRec.Monad0()))(Data_Functor.map(dictFunctor2)(function (t) {
                        return Control_Monad_Free_Trans.freeT(function (v3) {
                          return go(t);
                        });
                      })(zap(Data_Tuple.Tuple.create)(v1)(v2)));
                    });
                  }));
                };

                return Control_Monad_Free_Trans.freeT(function (v) {
                  return go(new Data_Tuple.Tuple(fs, gs));
                });
              };
            };
          };
        };
      };
    };
  };

  var fuseWith = function fuseWith(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictFunctor2) {
        return function (dictMonadRec) {
          return function (dictParallel) {
            return function (zap) {
              return function (fs) {
                return function (gs) {
                  var go = function go(v) {
                    return Control_Bind.bind(dictMonadRec.Monad0().Bind1())(Control_Parallel_Class.sequential(dictParallel)(Control_Apply.apply(dictParallel.Applicative1().Apply0())(Data_Functor.map(dictParallel.Applicative1().Apply0().Functor0())(Control_Apply.lift2(Data_Either.applyEither)(zap(Data_Tuple.Tuple.create)))(Control_Parallel_Class.parallel(dictParallel)(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0))))(Control_Parallel_Class.parallel(dictParallel)(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1)))))(function (v1) {
                      if (v1 instanceof Data_Either.Left) {
                        return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Data_Either.Left(v1.value0));
                      }

                      ;

                      if (v1 instanceof Data_Either.Right) {
                        return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Data_Either.Right(Data_Functor.map(dictFunctor2)(function (t) {
                          return Control_Monad_Free_Trans.freeT(function (v2) {
                            return go(t);
                          });
                        })(v1.value0)));
                      }

                      ;
                      throw new Error("Failed pattern match at Control.Coroutine (line 79, column 5 - line 81, column 63): " + [v1.constructor.name]);
                    });
                  };

                  return Control_Monad_Free_Trans.freeT(function (v) {
                    return go(new Data_Tuple.Tuple(fs, gs));
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  var functorAwait = new Data_Functor.Functor(Data_Profunctor.rmap(profunctorAwait));
  var bifunctorEmit = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        return new Emit(f(v.value0), g(v.value1));
      };
    };
  });
  var functorEmit = new Data_Functor.Functor(Data_Bifunctor.rmap(bifunctorEmit));

  var connect = function connect(dictMonadRec) {
    return function (dictParallel) {
      return fuseWith(functorEmit)(functorAwait)(Data_Identity.functorIdentity)(dictMonadRec)(dictParallel)(function (f) {
        return function (v) {
          return function (v1) {
            return f(v.value1)(v1(v.value0));
          };
        };
      });
    };
  };

  var emit = function emit(dictMonad) {
    return function (o) {
      return Control_Monad_Free_Trans.liftFreeT(functorEmit)(dictMonad)(new Emit(o, Data_Unit.unit));
    };
  };

  var producer = function producer(dictMonad) {
    return function (recv) {
      return loop(functorEmit)(dictMonad)(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(functorEmit)(dictMonad))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(functorEmit))(dictMonad)(recv))(function (v) {
        if (v instanceof Data_Either.Left) {
          return Data_Functor.voidLeft(Control_Monad_Free_Trans.functorFreeT(functorEmit)(dictMonad.Bind1().Apply0().Functor0()))(emit(dictMonad)(v.value0))(Data_Maybe.Nothing.value);
        }

        ;

        if (v instanceof Data_Either.Right) {
          return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(functorEmit)(dictMonad))(new Data_Maybe.Just(v.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Coroutine (line 125, column 3 - line 127, column 29): " + [v.constructor.name]);
      }));
    };
  };

  var pullFrom = function pullFrom(dictMonadRec) {
    return fuseWithL(functorAwait)(functorEmit)(Data_Identity.functorIdentity)(dictMonadRec)(function (f) {
      return function (v) {
        return function (v1) {
          return Control_Applicative.pure(Data_Identity.applicativeIdentity)(f(v(v1.value0))(v1.value1));
        };
      };
    });
  };

  var $$await = function $$await(dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorAwait)(dictMonad)(Control_Category.identity(Control_Category.categoryFn));
  };

  exports["runProcess"] = runProcess;
  exports["producer"] = producer;
  exports["await"] = $$await;
  exports["connect"] = connect;
  exports["pullFrom"] = pullFrom;
  exports["bifunctorEmit"] = bifunctorEmit;
  exports["functorEmit"] = functorEmit;
  exports["functorAwait"] = functorAwait;
})(PS);

(function (exports) {
  /* globals exports, setTimeout */
  "use strict";

  var AVar = function () {
    function MutableQueue() {
      this.head = null;
      this.last = null;
      this.size = 0;
    }

    function MutableCell(queue, value) {
      this.queue = queue;
      this.value = value;
      this.next = null;
      this.prev = null;
    }

    function AVar(value) {
      this.draining = false;
      this.error = null;
      this.value = value;
      this.takes = new MutableQueue();
      this.reads = new MutableQueue();
      this.puts = new MutableQueue();
    }

    var EMPTY = {};

    function runEff(eff) {
      try {
        eff();
      } catch (error) {
        setTimeout(function () {
          throw error;
        }, 0);
      }
    }

    function putLast(queue, value) {
      var cell = new MutableCell(queue, value);

      switch (queue.size) {
        case 0:
          queue.head = cell;
          break;

        case 1:
          cell.prev = queue.head;
          queue.head.next = cell;
          queue.last = cell;
          break;

        default:
          cell.prev = queue.last;
          queue.last.next = cell;
          queue.last = cell;
      }

      queue.size++;
      return cell;
    }

    function takeLast(queue) {
      var cell;

      switch (queue.size) {
        case 0:
          return null;

        case 1:
          cell = queue.head;
          queue.head = null;
          break;

        case 2:
          cell = queue.last;
          queue.head.next = null;
          queue.last = null;
          break;

        default:
          cell = queue.last;
          queue.last = cell.prev;
          queue.last.next = null;
      }

      cell.prev = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }

    function takeHead(queue) {
      var cell;

      switch (queue.size) {
        case 0:
          return null;

        case 1:
          cell = queue.head;
          queue.head = null;
          break;

        case 2:
          cell = queue.head;
          queue.last.prev = null;
          queue.head = queue.last;
          queue.last = null;
          break;

        default:
          cell = queue.head;
          queue.head = cell.next;
          queue.head.prev = null;
      }

      cell.next = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }

    function deleteCell(cell) {
      if (cell.queue === null) {
        return;
      }

      if (cell.queue.last === cell) {
        takeLast(cell.queue);
        return;
      }

      if (cell.queue.head === cell) {
        takeHead(cell.queue);
        return;
      }

      if (cell.prev) {
        cell.prev.next = cell.next;
      }

      if (cell.next) {
        cell.next.prev = cell.prev;
      }

      cell.queue.size--;
      cell.queue = null;
      cell.value = null;
      cell.next = null;
      cell.prev = null;
    }

    function drainVar(util, avar) {
      if (avar.draining) {
        return;
      }

      var ps = avar.puts;
      var ts = avar.takes;
      var rs = avar.reads;
      var p, r, t, value, rsize;
      avar.draining = true;

      while (1) {
        // eslint-disable-line no-constant-condition
        p = null;
        r = null;
        t = null;
        value = avar.value;
        rsize = rs.size;

        if (avar.error !== null) {
          value = util.left(avar.error);

          while (p = takeHead(ps)) {
            // eslint-disable-line no-cond-assign
            runEff(p.cb(value));
          }

          while (r = takeHead(rs)) {
            // eslint-disable-line no-cond-assign
            runEff(r(value));
          }

          while (t = takeHead(ts)) {
            // eslint-disable-line no-cond-assign
            runEff(t(value));
          }

          break;
        } // Process the next put. We do not immediately invoke the callback
        // because we want to preserve ordering. If there are takes/reads
        // we want to run those first.


        if (value === EMPTY && (p = takeHead(ps))) {
          avar.value = value = p.value;
        }

        if (value !== EMPTY) {
          // We go ahead and queue up the next take for the same reasons as
          // above. Invoking the read callbacks can affect the mutable queue.
          t = takeHead(ts); // We only want to process the reads queued up before running these
          // callbacks so we guard on rsize.

          while (rsize-- && (r = takeHead(rs))) {
            runEff(r(util.right(value)));
          }

          if (t !== null) {
            avar.value = EMPTY;
            runEff(t(util.right(value)));
          }
        }

        if (p !== null) {
          runEff(p.cb(util.right(void 0)));
        } // Callbacks could have queued up more items so we need to guard on the
        // actual mutable properties.


        if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) {
          break;
        }
      }

      avar.draining = false;
    }

    AVar.EMPTY = EMPTY;
    AVar.putLast = putLast;
    AVar.takeLast = takeLast;
    AVar.takeHead = takeHead;
    AVar.deleteCell = deleteCell;
    AVar.drainVar = drainVar;
    return AVar;
  }();

  exports.empty = function () {
    return new AVar(AVar.EMPTY);
  };

  exports._newVar = function (value) {
    return function () {
      return new AVar(value);
    };
  };

  exports._killVar = function (util, error, avar) {
    return function () {
      if (avar.error === null) {
        avar.error = error;
        avar.value = AVar.EMPTY;
        AVar.drainVar(util, avar);
      }
    };
  };

  exports._putVar = function (util, value, avar, cb) {
    return function () {
      var cell = AVar.putLast(avar.puts, {
        cb: cb,
        value: value
      });
      AVar.drainVar(util, avar);
      return function () {
        AVar.deleteCell(cell);
      };
    };
  };

  exports._takeVar = function (util, avar, cb) {
    return function () {
      var cell = AVar.putLast(avar.takes, cb);
      AVar.drainVar(util, avar);
      return function () {
        AVar.deleteCell(cell);
      };
    };
  };
})(PS["Effect.AVar"] = PS["Effect.AVar"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.AVar"] = $PS["Effect.AVar"] || {};
  var exports = $PS["Effect.AVar"];
  var $foreign = $PS["Effect.AVar"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Killed = function () {
    function Killed(value0) {
      this.value0 = value0;
    }

    ;

    Killed.create = function (value0) {
      return new Killed(value0);
    };

    return Killed;
  }();

  var Filled = function () {
    function Filled(value0) {
      this.value0 = value0;
    }

    ;

    Filled.create = function (value0) {
      return new Filled(value0);
    };

    return Filled;
  }();

  var Empty = function () {
    function Empty() {}

    ;
    Empty.value = new Empty();
    return Empty;
  }();

  var $$new = $foreign["_newVar"];
  var ffiUtil = {
    left: Data_Either.Left.create,
    right: Data_Either.Right.create,
    nothing: Data_Maybe.Nothing.value,
    just: Data_Maybe.Just.create,
    killed: Killed.create,
    filled: Filled.create,
    empty: Empty.value
  };

  var kill = function kill(err) {
    return function (avar) {
      return $foreign["_killVar"](ffiUtil, err, avar);
    };
  };

  var put = function put(value) {
    return function (avar) {
      return function (cb) {
        return $foreign["_putVar"](ffiUtil, value, avar, cb);
      };
    };
  };

  var take = function take(avar) {
    return function (cb) {
      return $foreign["_takeVar"](ffiUtil, avar, cb);
    };
  };

  exports["new"] = $$new;
  exports["take"] = take;
  exports["put"] = put;
  exports["kill"] = kill;
  exports["empty"] = $foreign.empty;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Aff.AVar"] = $PS["Effect.Aff.AVar"] || {};
  var exports = $PS["Effect.Aff.AVar"];
  var Effect_AVar = $PS["Effect.AVar"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];

  var take = function take(avar) {
    return Effect_Aff.makeAff(function (k) {
      return function __do() {
        var v = Effect_AVar.take(avar)(k)();
        return Effect_Aff.effectCanceler(v);
      };
    });
  };

  var put = function put(value) {
    return function (avar) {
      return Effect_Aff.makeAff(function (k) {
        return function __do() {
          var v = Effect_AVar.put(value)(avar)(k)();
          return Effect_Aff.effectCanceler(v);
        };
      });
    };
  };

  var kill = function kill(error) {
    var $17 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
    var $18 = Effect_AVar.kill(error);
    return function ($19) {
      return $17($18($19));
    };
  };

  var empty = Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_AVar.empty);
  exports["empty"] = empty;
  exports["take"] = take;
  exports["put"] = put;
  exports["kill"] = kill;
})(PS);

(function (exports) {
  "use strict";

  exports.error = function (msg) {
    return new Error(msg);
  };

  exports.throwException = function (e) {
    return function () {
      throw e;
    };
  };
})(PS["Effect.Exception"] = PS["Effect.Exception"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Exception"] = $PS["Effect.Exception"] || {};
  var exports = $PS["Effect.Exception"];
  var $foreign = $PS["Effect.Exception"];

  var $$throw = function $$throw($2) {
    return $foreign.throwException($foreign.error($2));
  };

  exports["throw"] = $$throw;
  exports["error"] = $foreign.error;
  exports["throwException"] = $foreign.throwException;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query.EventSource"] = $PS["Halogen.Query.EventSource"] || {};
  var exports = $PS["Halogen.Query.EventSource"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Coroutine = $PS["Control.Coroutine"];
  var Control_Monad_Free_Trans = $PS["Control.Monad.Free.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_AVar = $PS["Effect.Aff.AVar"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Exception = $PS["Effect.Exception"];

  var EventSource = function EventSource(x) {
    return x;
  };

  var hoistFinalizer = function hoistFinalizer(nat) {
    return function (v) {
      return nat(v);
    };
  };

  var hoist = function hoist(dictFunctor) {
    return function (nat) {
      return function (v) {
        return EventSource(Data_Functor.map(dictFunctor)(function (e) {
          return {
            producer: Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine.functorEmit)(dictFunctor)(nat)(e.producer),
            finalizer: hoistFinalizer(nat)(e.finalizer)
          };
        })(nat(v)));
      };
    };
  };

  var functorEventSource = function functorEventSource(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return function (v) {
        return EventSource(Data_Functor.map(dictFunctor)(function (e) {
          return {
            producer: Control_Monad_Free_Trans.interpret(Control_Coroutine.functorEmit)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine.bifunctorEmit)(f))(e.producer),
            finalizer: e.finalizer
          };
        })(v));
      };
    });
  };

  var finalize = function finalize(v) {
    return v;
  };

  var emit = function emit(v) {
    return function (a) {
      return v(new Data_Either.Left(a));
    };
  };

  var affEventSource = function affEventSource(dictMonadAff) {
    return function (recv) {
      return EventSource(Effect_Aff_Class.liftAff(dictMonadAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.empty)(function (v) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.empty)(function (v1) {
          var producer = Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorEmit)(dictMonadAff.MonadEffect0().Monad0()))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorEmit))(dictMonadAff.MonadEffect0().Monad0())(Effect_Aff_Class.liftAff(dictMonadAff)(Control_Bind.bindFlipped(Effect_Aff.bindAff)(Data_Function.flip(Effect_Aff_AVar.put)(v1))(recv(Data_Function.flip(Effect_Aff_AVar.put)(v))))))(function () {
            return Control_Coroutine.producer(dictMonadAff.MonadEffect0().Monad0())(Effect_Aff_Class.liftAff(dictMonadAff)(Effect_Aff_AVar.take(v)));
          });
          var finalizer = Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Effect_Aff_Class.liftAff(dictMonadAff)(Effect_Aff.attempt(Effect_Aff_AVar.take(v1))))(function (v2) {
            if (v2 instanceof Data_Either.Left) {
              return Control_Applicative.pure(dictMonadAff.MonadEffect0().Monad0().Applicative0())(Data_Unit.unit);
            }

            ;

            if (v2 instanceof Data_Either.Right) {
              return Effect_Aff_Class.liftAff(dictMonadAff)(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff_AVar.kill(Effect_Exception.error("finalized"))(v1))(function () {
                return finalize(v2.value0);
              }));
            }

            ;
            throw new Error("Failed pattern match at Halogen.Query.EventSource (line 71, column 51 - line 75, column 21): " + [v2.constructor.name]);
          });
          return Control_Applicative.pure(Effect_Aff.applicativeAff)({
            producer: producer,
            finalizer: finalizer
          });
        });
      })));
    };
  };

  exports["hoist"] = hoist;
  exports["affEventSource"] = affEventSource;
  exports["emit"] = emit;
  exports["finalize"] = finalize;
  exports["functorEventSource"] = functorEventSource;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query.HalogenM"] = $PS["Halogen.Query.HalogenM"] || {};
  var exports = $PS["Halogen.Query.HalogenM"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Applicative_Free = $PS["Control.Applicative.Free"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Free = $PS["Control.Monad.Free"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Class = $PS["Effect.Class"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_ChildQuery = $PS["Halogen.Query.ChildQuery"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];

  var SubscriptionId = function SubscriptionId(x) {
    return x;
  };

  var ForkId = function ForkId(x) {
    return x;
  };

  var State = function () {
    function State(value0) {
      this.value0 = value0;
    }

    ;

    State.create = function (value0) {
      return new State(value0);
    };

    return State;
  }();

  var Subscribe = function () {
    function Subscribe(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Subscribe.create = function (value0) {
      return function (value1) {
        return new Subscribe(value0, value1);
      };
    };

    return Subscribe;
  }();

  var Unsubscribe = function () {
    function Unsubscribe(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Unsubscribe.create = function (value0) {
      return function (value1) {
        return new Unsubscribe(value0, value1);
      };
    };

    return Unsubscribe;
  }();

  var Lift = function () {
    function Lift(value0) {
      this.value0 = value0;
    }

    ;

    Lift.create = function (value0) {
      return new Lift(value0);
    };

    return Lift;
  }();

  var ChildQuery = function () {
    function ChildQuery(value0) {
      this.value0 = value0;
    }

    ;

    ChildQuery.create = function (value0) {
      return new ChildQuery(value0);
    };

    return ChildQuery;
  }();

  var Raise = function () {
    function Raise(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Raise.create = function (value0) {
      return function (value1) {
        return new Raise(value0, value1);
      };
    };

    return Raise;
  }();

  var Par = function () {
    function Par(value0) {
      this.value0 = value0;
    }

    ;

    Par.create = function (value0) {
      return new Par(value0);
    };

    return Par;
  }();

  var Fork = function () {
    function Fork(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Fork.create = function (value0) {
      return function (value1) {
        return new Fork(value0, value1);
      };
    };

    return Fork;
  }();

  var Kill = function () {
    function Kill(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Kill.create = function (value0) {
      return function (value1) {
        return new Kill(value0, value1);
      };
    };

    return Kill;
  }();

  var GetRef = function () {
    function GetRef(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    GetRef.create = function (value0) {
      return function (value1) {
        return new GetRef(value0, value1);
      };
    };

    return GetRef;
  }();

  var HalogenAp = function HalogenAp(x) {
    return x;
  };

  var HalogenM = function HalogenM(x) {
    return x;
  };

  var subscribe = function subscribe(es) {
    return HalogenM(Control_Monad_Free.liftF(new Subscribe(function (v) {
      return es;
    }, Control_Category.identity(Control_Category.categoryFn))));
  };

  var raise = function raise(o) {
    return HalogenM(Control_Monad_Free.liftF(new Raise(o, Data_Unit.unit)));
  };

  var query = function query(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (q) {
              return HalogenM(Control_Monad_Free.liftF(ChildQuery.create(Halogen_Query_ChildQuery.mkChildQueryBox(new Halogen_Query_ChildQuery.ChildQuery(function (dictApplicative) {
                return function (k) {
                  var $133 = Data_Maybe.maybe(Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value))(k);
                  var $134 = Halogen_Data_Slot.lookup()(dictIsSymbol)(dictOrd)(label)(p);
                  return function ($135) {
                    return $133($134($135));
                  };
                };
              }, q, Control_Category.identity(Control_Category.categoryFn))))));
            };
          };
        };
      };
    };
  };

  var ordSubscriptionId = Data_Ord.ordInt;
  var ordForkId = Data_Ord.ordInt;
  var newtypeHalogenAp = new Data_Newtype.Newtype(function (n) {
    return n;
  }, HalogenAp);
  var monadTransHalogenM = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function ($136) {
      return HalogenM(Control_Monad_Free.liftF(Lift.create($136)));
    };
  });
  var monadHalogenM = Control_Monad_Free.freeMonad;
  var monadStateHalogenM = new Control_Monad_State_Class.MonadState(function () {
    return monadHalogenM;
  }, function ($137) {
    return HalogenM(Control_Monad_Free.liftF(State.create($137)));
  });

  var monadEffectHalogenM = function monadEffectHalogenM(dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
      return monadHalogenM;
    }, function () {
      var $142 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($143) {
        return HalogenM(Control_Monad_Free.liftF(Lift.create($142($143))));
      };
    }());
  };

  var monadAskHalogenM = function monadAskHalogenM(dictMonadAsk) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
      return monadHalogenM;
    }, HalogenM(Control_Monad_Free.liftF(new Lift(Control_Monad_Reader_Class.ask(dictMonadAsk)))));
  };

  var monadAffHalogenM = function monadAffHalogenM(dictMonadAff) {
    return new Effect_Aff_Class.MonadAff(function () {
      return monadEffectHalogenM(dictMonadAff.MonadEffect0());
    }, function () {
      var $144 = Effect_Aff_Class.liftAff(dictMonadAff);
      return function ($145) {
        return HalogenM(Control_Monad_Free.liftF(Lift.create($144($145))));
      };
    }());
  };

  var kill = function kill(fid) {
    return HalogenM(Control_Monad_Free.liftF(new Kill(fid, Data_Unit.unit)));
  };

  var hoist = function hoist(dictFunctor) {
    return function (nat) {
      return function (v) {
        var go = function go(v1) {
          if (v1 instanceof State) {
            return new State(v1.value0);
          }

          ;

          if (v1 instanceof Subscribe) {
            return new Subscribe(function () {
              var $150 = Halogen_Query_EventSource.hoist(dictFunctor)(nat);
              return function ($151) {
                return $150(v1.value0($151));
              };
            }(), v1.value1);
          }

          ;

          if (v1 instanceof Unsubscribe) {
            return new Unsubscribe(v1.value0, v1.value1);
          }

          ;

          if (v1 instanceof Lift) {
            return new Lift(nat(v1.value0));
          }

          ;

          if (v1 instanceof ChildQuery) {
            return new ChildQuery(v1.value0);
          }

          ;

          if (v1 instanceof Raise) {
            return new Raise(v1.value0, v1.value1);
          }

          ;

          if (v1 instanceof Par) {
            return new Par(Data_Newtype.over(newtypeHalogenAp)(newtypeHalogenAp)(HalogenAp)(Control_Applicative_Free.hoistFreeAp(hoist(dictFunctor)(nat)))(v1.value0));
          }

          ;

          if (v1 instanceof Fork) {
            return new Fork(hoist(dictFunctor)(nat)(v1.value0), v1.value1);
          }

          ;

          if (v1 instanceof Kill) {
            return new Kill(v1.value0, v1.value1);
          }

          ;

          if (v1 instanceof GetRef) {
            return new GetRef(v1.value0, v1.value1);
          }

          ;
          throw new Error("Failed pattern match at Halogen.Query.HalogenM (line 279, column 8 - line 289, column 29): " + [v1.constructor.name]);
        };

        return Control_Monad_Free.hoistFree(go)(v);
      };
    };
  };

  var functorHalogenM = Control_Monad_Free.freeFunctor;

  var fork = function fork(hmu) {
    return HalogenM(Control_Monad_Free.liftF(new Fork(hmu, Control_Category.identity(Control_Category.categoryFn))));
  };

  var bindHalogenM = Control_Monad_Free.freeBind;
  var applyHalogenM = Control_Monad_Free.freeApply;
  var applicativeHalogenM = Control_Monad_Free.freeApplicative;
  exports["State"] = State;
  exports["Subscribe"] = Subscribe;
  exports["Unsubscribe"] = Unsubscribe;
  exports["Lift"] = Lift;
  exports["ChildQuery"] = ChildQuery;
  exports["Raise"] = Raise;
  exports["Par"] = Par;
  exports["Fork"] = Fork;
  exports["Kill"] = Kill;
  exports["GetRef"] = GetRef;
  exports["HalogenM"] = HalogenM;
  exports["raise"] = raise;
  exports["query"] = query;
  exports["SubscriptionId"] = SubscriptionId;
  exports["subscribe"] = subscribe;
  exports["ForkId"] = ForkId;
  exports["fork"] = fork;
  exports["kill"] = kill;
  exports["hoist"] = hoist;
  exports["functorHalogenM"] = functorHalogenM;
  exports["applyHalogenM"] = applyHalogenM;
  exports["applicativeHalogenM"] = applicativeHalogenM;
  exports["bindHalogenM"] = bindHalogenM;
  exports["monadHalogenM"] = monadHalogenM;
  exports["monadEffectHalogenM"] = monadEffectHalogenM;
  exports["monadAffHalogenM"] = monadAffHalogenM;
  exports["monadTransHalogenM"] = monadTransHalogenM;
  exports["monadStateHalogenM"] = monadStateHalogenM;
  exports["monadAskHalogenM"] = monadAskHalogenM;
  exports["ordSubscriptionId"] = ordSubscriptionId;
  exports["ordForkId"] = ordForkId;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query.HalogenQ"] = $PS["Halogen.Query.HalogenQ"] || {};
  var exports = $PS["Halogen.Query.HalogenQ"];

  var Initialize = function () {
    function Initialize(value0) {
      this.value0 = value0;
    }

    ;

    Initialize.create = function (value0) {
      return new Initialize(value0);
    };

    return Initialize;
  }();

  var Finalize = function () {
    function Finalize(value0) {
      this.value0 = value0;
    }

    ;

    Finalize.create = function (value0) {
      return new Finalize(value0);
    };

    return Finalize;
  }();

  var Receive = function () {
    function Receive(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Receive.create = function (value0) {
      return function (value1) {
        return new Receive(value0, value1);
      };
    };

    return Receive;
  }();

  var Action = function () {
    function Action(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Action.create = function (value0) {
      return function (value1) {
        return new Action(value0, value1);
      };
    };

    return Action;
  }();

  var Query = function () {
    function Query(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Query.create = function (value0) {
      return function (value1) {
        return new Query(value0, value1);
      };
    };

    return Query;
  }();

  exports["Initialize"] = Initialize;
  exports["Finalize"] = Finalize;
  exports["Receive"] = Receive;
  exports["Action"] = Action;
  exports["Query"] = Query;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.Machine"] = $PS["Halogen.VDom.Machine"] || {};
  var exports = $PS["Halogen.VDom.Machine"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Step = function () {
    function Step(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Step.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Step(value0, value1, value2, value3);
          };
        };
      };
    };

    return Step;
  }();

  var unStep = Unsafe_Coerce.unsafeCoerce;

  var step = function step(v, a) {
    return v.value2(v.value1, a);
  };

  var mkStep = Unsafe_Coerce.unsafeCoerce;

  var halt = function halt(v) {
    return v.value3(v.value1);
  };

  var extract = unStep(function (v) {
    return v.value0;
  });
  exports["Step"] = Step;
  exports["mkStep"] = mkStep;
  exports["unStep"] = unStep;
  exports["extract"] = extract;
  exports["step"] = step;
  exports["halt"] = halt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.Types"] = $PS["Halogen.VDom.Types"] || {};
  var exports = $PS["Halogen.VDom.Types"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Text = function () {
    function Text(value0) {
      this.value0 = value0;
    }

    ;

    Text.create = function (value0) {
      return new Text(value0);
    };

    return Text;
  }();

  var Elem = function () {
    function Elem(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Elem.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Elem(value0, value1, value2, value3);
          };
        };
      };
    };

    return Elem;
  }();

  var Keyed = function () {
    function Keyed(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Keyed.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Keyed(value0, value1, value2, value3);
          };
        };
      };
    };

    return Keyed;
  }();

  var Widget = function () {
    function Widget(value0) {
      this.value0 = value0;
    }

    ;

    Widget.create = function (value0) {
      return new Widget(value0);
    };

    return Widget;
  }();

  var Grafted = function () {
    function Grafted(value0) {
      this.value0 = value0;
    }

    ;

    Grafted.create = function (value0) {
      return new Grafted(value0);
    };

    return Grafted;
  }();

  var Graft = function () {
    function Graft(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Graft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Graft(value0, value1, value2);
        };
      };
    };

    return Graft;
  }();

  var unGraft = function unGraft(f) {
    return function ($58) {
      return f($58);
    };
  };

  var graft = Unsafe_Coerce.unsafeCoerce;
  var bifunctorGraft = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return unGraft(function (v) {
        return graft(new Graft(function ($60) {
          return f(v.value0($60));
        }, function ($61) {
          return g(v.value1($61));
        }, v.value2));
      });
    };
  });
  var bifunctorVDom = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        if (v instanceof Text) {
          return new Text(v.value0);
        }

        ;

        if (v instanceof Grafted) {
          return new Grafted(Data_Bifunctor.bimap(bifunctorGraft)(f)(g)(v.value0));
        }

        ;
        return new Grafted(graft(new Graft(f, g, v)));
      };
    };
  });
  var runGraft = unGraft(function (v) {
    var go = function go(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }

      ;

      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), Data_Functor.map(Data_Functor.functorArray)(go)(v2.value3));
      }

      ;

      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Tuple.functorTuple)(go))(v2.value3));
      }

      ;

      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }

      ;

      if (v2 instanceof Grafted) {
        return new Grafted(Data_Bifunctor.bimap(bifunctorGraft)(v.value0)(v.value1)(v2.value0));
      }

      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };

    return go(v.value2);
  });
  exports["Text"] = Text;
  exports["Elem"] = Elem;
  exports["Keyed"] = Keyed;
  exports["Widget"] = Widget;
  exports["Grafted"] = Grafted;
  exports["runGraft"] = runGraft;
  exports["bifunctorVDom"] = bifunctorVDom;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeGetAny = function (key, obj) {
    return obj[key];
  };

  exports.unsafeHasAny = function (key, obj) {
    return obj.hasOwnProperty(key);
  };

  exports.unsafeSetAny = function (key, val, obj) {
    obj[key] = val;
  };

  exports.forE = function (a, f) {
    var b = [];

    for (var i = 0; i < a.length; i++) {
      b.push(f(i, a[i]));
    }

    return b;
  };

  exports.forEachE = function (a, f) {
    for (var i = 0; i < a.length; i++) {
      f(a[i]);
    }
  };

  exports.forInE = function (o, f) {
    var ks = Object.keys(o);

    for (var i = 0; i < ks.length; i++) {
      var k = ks[i];
      f(k, o[k]);
    }
  };

  exports.diffWithIxE = function (a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i = 0;

    while (1) {
      if (i < l1) {
        if (i < l2) {
          a3.push(f1(i, a1[i], a2[i]));
        } else {
          f2(i, a1[i]);
        }
      } else if (i < l2) {
        a3.push(f3(i, a2[i]));
      } else {
        break;
      }

      i++;
    }

    return a3;
  };

  exports.strMapWithIxE = function (as, fk, f) {
    var o = {};

    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      o[k] = f(k, i, a);
    }

    return o;
  };

  exports.diffWithKeyAndIxE = function (o1, as, fk, f1, f2, f3) {
    var o2 = {};

    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);

      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i, o1[k], a);
      } else {
        o2[k] = f3(k, i, a);
      }
    }

    for (var k in o1) {
      if (k in o2) {
        continue;
      }

      f2(k, o1[k]);
    }

    return o2;
  };

  exports.refEq = function (a, b) {
    return a === b;
  };

  exports.createTextNode = function (s, doc) {
    return doc.createTextNode(s);
  };

  exports.setTextContent = function (s, n) {
    n.textContent = s;
  };

  exports.createElement = function (ns, name, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name);
    } else {
      return doc.createElement(name);
    }
  };

  exports.insertChildIx = function (i, a, b) {
    var n = b.childNodes.item(i) || null;

    if (n !== a) {
      b.insertBefore(a, n);
    }
  };

  exports.removeChild = function (a, b) {
    if (b && a.parentNode === b) {
      b.removeChild(a);
    }
  };

  exports.parentNode = function (a) {
    return a.parentNode;
  };

  exports.setAttribute = function (ns, attr, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr, val);
    } else {
      el.setAttribute(attr, val);
    }
  };

  exports.removeAttribute = function (ns, attr, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr);
    } else {
      el.removeAttribute(attr);
    }
  };

  exports.addEventListener = function (ev, listener, el) {
    el.addEventListener(ev, listener, false);
  };

  exports.removeEventListener = function (ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  };

  exports.jsUndefined = void 0;
})(PS["Halogen.VDom.Util"] = PS["Halogen.VDom.Util"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.Util"] = $PS["Halogen.VDom.Util"] || {};
  var exports = $PS["Halogen.VDom.Util"];
  var $foreign = $PS["Halogen.VDom.Util"];
  var Foreign_Object_ST = $PS["Foreign.Object.ST"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var unsafeLookup = $foreign.unsafeGetAny;
  var unsafeFreeze = Unsafe_Coerce.unsafeCoerce;
  var pokeMutMap = $foreign.unsafeSetAny;
  var newMutMap = Foreign_Object_ST["new"];
  exports["newMutMap"] = newMutMap;
  exports["pokeMutMap"] = pokeMutMap;
  exports["unsafeFreeze"] = unsafeFreeze;
  exports["unsafeLookup"] = unsafeLookup;
  exports["unsafeGetAny"] = $foreign.unsafeGetAny;
  exports["unsafeHasAny"] = $foreign.unsafeHasAny;
  exports["unsafeSetAny"] = $foreign.unsafeSetAny;
  exports["forE"] = $foreign.forE;
  exports["forEachE"] = $foreign.forEachE;
  exports["forInE"] = $foreign.forInE;
  exports["diffWithIxE"] = $foreign.diffWithIxE;
  exports["diffWithKeyAndIxE"] = $foreign.diffWithKeyAndIxE;
  exports["strMapWithIxE"] = $foreign.strMapWithIxE;
  exports["refEq"] = $foreign.refEq;
  exports["createTextNode"] = $foreign.createTextNode;
  exports["setTextContent"] = $foreign.setTextContent;
  exports["createElement"] = $foreign.createElement;
  exports["insertChildIx"] = $foreign.insertChildIx;
  exports["removeChild"] = $foreign.removeChild;
  exports["parentNode"] = $foreign.parentNode;
  exports["setAttribute"] = $foreign.setAttribute;
  exports["removeAttribute"] = $foreign.removeAttribute;
  exports["addEventListener"] = $foreign.addEventListener;
  exports["removeEventListener"] = $foreign.removeEventListener;
  exports["jsUndefined"] = $foreign.jsUndefined;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.DOM.Element"] = $PS["Web.DOM.Element"] || {};
  var exports = $PS["Web.DOM.Element"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toNode = Unsafe_Coerce.unsafeCoerce;
  exports["toNode"] = toNode;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.DOM"] = $PS["Halogen.VDom.DOM"] || {};
  var exports = $PS["Halogen.VDom.DOM"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Types = $PS["Halogen.VDom.Types"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];
  var Web_DOM_Element = $PS["Web.DOM.Element"];

  var haltWidget = function haltWidget(v) {
    return Halogen_VDom_Machine.halt(v.widget);
  };

  var patchWidget = function patchWidget(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchWidget(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Widget) {
      var v = Halogen_VDom_Machine.step(state.widget, vdom.value0);
      var res$prime = Halogen_VDom_Machine.unStep(function (v1) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(v1.value0, {
          build: state.build,
          widget: v
        }, patchWidget, haltWidget));
      })(v);
      return res$prime;
    }

    ;
    haltWidget(state);
    return state.build(vdom);
  };

  var haltText = function haltText(v) {
    var v1 = Halogen_VDom_Util.parentNode(v.node);
    return Halogen_VDom_Util.removeChild(v.node, v1);
  };

  var patchText = function patchText(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchText(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Text) {
      if (state.value === vdom.value0) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, state, patchText, haltText));
      }

      ;

      if (Data_Boolean.otherwise) {
        var nextState = {
          build: state.build,
          node: state.node,
          value: vdom.value0
        };
        Halogen_VDom_Util.setTextContent(vdom.value0, state.node);
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchText, haltText));
      }

      ;
    }

    ;
    haltText(state);
    return state.build(vdom);
  };

  var haltKeyed = function haltKeyed(v) {
    var v1 = Halogen_VDom_Util.parentNode(v.node);
    Halogen_VDom_Util.removeChild(v.node, v1);
    Halogen_VDom_Util.forInE(v.children, function (v2, s) {
      return Halogen_VDom_Machine.halt(s);
    });
    return Halogen_VDom_Machine.halt(v.attrs);
  };

  var haltElem = function haltElem(v) {
    var v1 = Halogen_VDom_Util.parentNode(v.node);
    Halogen_VDom_Util.removeChild(v.node, v1);
    Halogen_VDom_Util.forEachE(v.children, Halogen_VDom_Machine.halt);
    return Halogen_VDom_Machine.halt(v.attrs);
  };

  var eqElemSpec = function eqElemSpec(ns1, v, ns2, v1) {
    var $84 = v === v1;

    if ($84) {
      if (ns1 instanceof Data_Maybe.Just && ns2 instanceof Data_Maybe.Just && ns1.value0 === ns2.value0) {
        return true;
      }

      ;

      if (ns1 instanceof Data_Maybe.Nothing && ns2 instanceof Data_Maybe.Nothing) {
        return true;
      }

      ;
      return false;
    }

    ;
    return false;
  };

  var patchElem = function patchElem(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchElem(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Elem && eqElemSpec(state.ns, state.name, vdom.value0, vdom.value1)) {
      var v = Data_Array.length(vdom.value3);
      var v1 = Data_Array.length(state.children);

      if (v1 === 0 && v === 0) {
        var v2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
        var nextState = {
          build: state.build,
          node: state.node,
          attrs: v2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state.children
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchElem, haltElem));
      }

      ;

      var onThis = function onThis(ix, s) {
        return Halogen_VDom_Machine.halt(s);
      };

      var onThese = function onThese(ix, s, v2) {
        var v3 = Halogen_VDom_Machine.step(s, v2);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(v3), state.node);
        return v3;
      };

      var onThat = function onThat(ix, v2) {
        var v3 = state.build(v2);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(v3), state.node);
        return v3;
      };

      var v2 = Halogen_VDom_Util.diffWithIxE(state.children, vdom.value3, onThese, onThis, onThat);
      var v3 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
      var nextState = {
        build: state.build,
        node: state.node,
        attrs: v3,
        ns: vdom.value0,
        name: vdom.value1,
        children: v2
      };
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchElem, haltElem));
    }

    ;
    haltElem(state);
    return state.build(vdom);
  };

  var patchKeyed = function patchKeyed(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchKeyed(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Keyed && eqElemSpec(state.ns, state.name, vdom.value0, vdom.value1)) {
      var v = Data_Array.length(vdom.value3);

      if (state.length === 0 && v === 0) {
        var v2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
        var nextState = {
          build: state.build,
          node: state.node,
          attrs: v2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state.children,
          length: 0
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchKeyed, haltKeyed));
      }

      ;

      var onThis = function onThis(v2, s) {
        return Halogen_VDom_Machine.halt(s);
      };

      var onThese = function onThese(v2, ix$prime, s, v3) {
        var v5 = Halogen_VDom_Machine.step(s, v3.value1);
        Halogen_VDom_Util.insertChildIx(ix$prime, Halogen_VDom_Machine.extract(v5), state.node);
        return v5;
      };

      var onThat = function onThat(v2, ix, v3) {
        var v5 = state.build(v3.value1);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(v5), state.node);
        return v5;
      };

      var v2 = Halogen_VDom_Util.diffWithKeyAndIxE(state.children, vdom.value3, Data_Tuple.fst, onThese, onThis, onThat);
      var v3 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
      var nextState = {
        build: state.build,
        node: state.node,
        attrs: v3,
        ns: vdom.value0,
        name: vdom.value1,
        children: v2,
        length: v
      };
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchKeyed, haltKeyed));
    }

    ;
    haltKeyed(state);
    return state.build(vdom);
  };

  var buildWidget = function buildWidget(v, build, w) {
    var v1 = v.buildWidget(v)(w);
    var res$prime = Halogen_VDom_Machine.unStep(function (v2) {
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(v2.value0, {
        build: build,
        widget: v1
      }, patchWidget, haltWidget));
    })(v1);
    return res$prime;
  };

  var buildText = function buildText(v, build, s) {
    var v1 = Halogen_VDom_Util.createTextNode(s, v.document);
    var state = {
      build: build,
      node: v1,
      value: s
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(v1, state, patchText, haltText));
  };

  var buildKeyed = function buildKeyed(v, build, ns1, name1, as1, ch1) {
    var v1 = Halogen_VDom_Util.createElement(Data_Nullable.toNullable(ns1), name1, v.document);
    var node = Web_DOM_Element.toNode(v1);

    var onChild = function onChild(k, ix, v2) {
      var v3 = build(v2.value1);
      Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(v3), node);
      return v3;
    };

    var v2 = Halogen_VDom_Util.strMapWithIxE(ch1, Data_Tuple.fst, onChild);
    var v3 = v.buildAttributes(v1)(as1);
    var state = {
      build: build,
      node: node,
      attrs: v3,
      ns: ns1,
      name: name1,
      children: v2,
      length: Data_Array.length(ch1)
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, state, patchKeyed, haltKeyed));
  };

  var buildElem = function buildElem(v, build, ns1, name1, as1, ch1) {
    var v1 = Halogen_VDom_Util.createElement(Data_Nullable.toNullable(ns1), name1, v.document);
    var node = Web_DOM_Element.toNode(v1);

    var onChild = function onChild(ix, child) {
      var v2 = build(child);
      Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(v2), node);
      return v2;
    };

    var v2 = Halogen_VDom_Util.forE(ch1, onChild);
    var v3 = v.buildAttributes(v1)(as1);
    var state = {
      build: build,
      node: node,
      attrs: v3,
      ns: ns1,
      name: name1,
      children: v2
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, state, patchElem, haltElem));
  };

  var buildVDom = function buildVDom(spec) {
    var build = function build(v) {
      if (v instanceof Halogen_VDom_Types.Text) {
        return buildText(spec, build, v.value0);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Elem) {
        return buildElem(spec, build, v.value0, v.value1, v.value2, v.value3);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Keyed) {
        return buildKeyed(spec, build, v.value0, v.value1, v.value2, v.value3);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Widget) {
        return buildWidget(spec, build, v.value0);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Grafted) {
        return build(Halogen_VDom_Types.runGraft(v.value0));
      }

      ;
      throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
    };

    return build;
  };

  exports["buildVDom"] = buildVDom;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.Thunk"] = $PS["Halogen.VDom.Thunk"] || {};
  var exports = $PS["Halogen.VDom.Thunk"];
  var Halogen_VDom_DOM = $PS["Halogen.VDom.DOM"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];

  var Thunk = function () {
    function Thunk(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Thunk.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Thunk(value0, value1, value2, value3);
          };
        };
      };
    };

    return Thunk;
  }();

  var unsafeEqThunk = function unsafeEqThunk(v, v1) {
    return Halogen_VDom_Util.refEq(v.value0, v1.value0) && Halogen_VDom_Util.refEq(v.value1, v1.value1) && Halogen_VDom_Util.refEq(v.value3, v1.value3);
  };

  var thunk = function thunk(tid, eqFn, f, a) {
    return new Thunk(tid, eqFn, f, a);
  };

  var runThunk = function runThunk(v) {
    return v.value2(v.value3);
  };

  var mapThunk = function mapThunk(k) {
    return function (v) {
      return new Thunk(v.value0, v.value1, function ($50) {
        return k(v.value2($50));
      }, v.value3);
    };
  };

  var hoist = mapThunk;

  var buildThunk = function buildThunk(toVDom) {
    var haltThunk = function haltThunk(state) {
      return Halogen_VDom_Machine.halt(state.vdom);
    };

    var patchThunk = function patchThunk(state, t2) {
      var $45 = unsafeEqThunk(state.thunk, t2);

      if ($45) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(state.vdom), state, patchThunk, haltThunk));
      }

      ;
      var v = Halogen_VDom_Machine.step(state.vdom, toVDom(runThunk(t2)));
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), {
        vdom: v,
        thunk: t2
      }, patchThunk, haltThunk));
    };

    var renderThunk = function renderThunk(spec) {
      return function (t) {
        var v = Halogen_VDom_DOM.buildVDom(spec)(toVDom(runThunk(t)));
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), {
          thunk: t,
          vdom: v
        }, patchThunk, haltThunk));
      };
    };

    return renderThunk;
  };

  exports["buildThunk"] = buildThunk;
  exports["hoist"] = hoist;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Component"] = $PS["Halogen.Component"] || {};
  var exports = $PS["Halogen.Component"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Halogen_VDom_Thunk = $PS["Halogen.VDom.Thunk"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var ComponentSlot = function () {
    function ComponentSlot(value0) {
      this.value0 = value0;
    }

    ;

    ComponentSlot.create = function (value0) {
      return new ComponentSlot(value0);
    };

    return ComponentSlot;
  }();

  var ThunkSlot = function () {
    function ThunkSlot(value0) {
      this.value0 = value0;
    }

    ;

    ThunkSlot.create = function (value0) {
      return new ThunkSlot(value0);
    };

    return ThunkSlot;
  }();

  var unComponentSlot = Unsafe_Coerce.unsafeCoerce;
  var unComponent = Unsafe_Coerce.unsafeCoerce;

  var mkEval = function mkEval(args) {
    return function (v) {
      if (v instanceof Halogen_Query_HalogenQ.Initialize) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.initialize))(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Finalize) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.finalize))(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Receive) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.receive(v.value0)))(v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Action) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(args.handleAction(v.value0))(v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Query) {
        return Data_Coyoneda.unCoyoneda(function (g) {
          var $28 = Data_Functor.map(Halogen_Query_HalogenM.functorHalogenM)(Data_Maybe.maybe(v.value1(Data_Unit.unit))(g));
          return function ($29) {
            return $28(args.handleQuery($29));
          };
        })(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Halogen.Component (line 172, column 15 - line 182, column 70): " + [v.constructor.name]);
    };
  };

  var mkComponentSlot = Unsafe_Coerce.unsafeCoerce;
  var mkComponent = Unsafe_Coerce.unsafeCoerce;

  var hoistSlot = function hoistSlot(dictBifunctor) {
    return function (dictFunctor) {
      return function (nat) {
        return function (v) {
          if (v instanceof ComponentSlot) {
            return unComponentSlot(function (slot) {
              return ComponentSlot.create(mkComponentSlot({
                get: slot.get,
                pop: slot.pop,
                set: slot.set,
                component: hoist(dictBifunctor)(dictFunctor)(nat)(slot.component),
                input: slot.input,
                output: slot.output
              }));
            })(v.value0);
          }

          ;

          if (v instanceof ThunkSlot) {
            return ThunkSlot.create(Halogen_VDom_Thunk.hoist(Data_Bifunctor.lmap(dictBifunctor)(hoistSlot(dictBifunctor)(dictFunctor)(nat)))(v.value0));
          }

          ;
          throw new Error("Failed pattern match at Halogen.Component (line 271, column 17 - line 276, column 53): " + [v.constructor.name]);
        };
      };
    };
  };

  var hoist = function hoist(dictBifunctor) {
    return function (dictFunctor) {
      return function (nat) {
        return unComponent(function (c) {
          return mkComponent({
            initialState: c.initialState,
            render: function () {
              var $30 = Data_Bifunctor.lmap(dictBifunctor)(hoistSlot(dictBifunctor)(dictFunctor)(nat));
              return function ($31) {
                return $30(c.render($31));
              };
            }(),
            "eval": function () {
              var $32 = Halogen_Query_HalogenM.hoist(dictFunctor)(nat);
              return function ($33) {
                return $32(c["eval"]($33));
              };
            }()
          });
        });
      };
    };
  };

  var defaultEval = {
    handleAction: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)),
    handleQuery: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Maybe.Nothing.value)),
    receive: Data_Function["const"](Data_Maybe.Nothing.value),
    initialize: Data_Maybe.Nothing.value,
    finalize: Data_Maybe.Nothing.value
  };

  var componentSlot = function componentSlot(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (comp) {
              return function (input) {
                return function (output) {
                  return mkComponentSlot({
                    get: Halogen_Data_Slot.lookup()(dictIsSymbol)(dictOrd)(label)(p),
                    pop: Halogen_Data_Slot.pop()(dictIsSymbol)(dictOrd)(label)(p),
                    set: Halogen_Data_Slot.insert()(dictIsSymbol)(dictOrd)(label)(p),
                    component: comp,
                    input: new Halogen_Query_HalogenQ.Receive(input, Data_Unit.unit),
                    output: output
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  exports["mkComponent"] = mkComponent;
  exports["unComponent"] = unComponent;
  exports["hoist"] = hoist;
  exports["mkEval"] = mkEval;
  exports["defaultEval"] = defaultEval;
  exports["ComponentSlot"] = ComponentSlot;
  exports["ThunkSlot"] = ThunkSlot;
  exports["componentSlot"] = componentSlot;
  exports["unComponentSlot"] = unComponentSlot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["DOM.HTML.Indexed.InputType"] = $PS["DOM.HTML.Indexed.InputType"] || {};
  var exports = $PS["DOM.HTML.Indexed.InputType"];

  var InputButton = function () {
    function InputButton() {}

    ;
    InputButton.value = new InputButton();
    return InputButton;
  }();

  var InputCheckbox = function () {
    function InputCheckbox() {}

    ;
    InputCheckbox.value = new InputCheckbox();
    return InputCheckbox;
  }();

  var InputColor = function () {
    function InputColor() {}

    ;
    InputColor.value = new InputColor();
    return InputColor;
  }();

  var InputDate = function () {
    function InputDate() {}

    ;
    InputDate.value = new InputDate();
    return InputDate;
  }();

  var InputDatetimeLocal = function () {
    function InputDatetimeLocal() {}

    ;
    InputDatetimeLocal.value = new InputDatetimeLocal();
    return InputDatetimeLocal;
  }();

  var InputEmail = function () {
    function InputEmail() {}

    ;
    InputEmail.value = new InputEmail();
    return InputEmail;
  }();

  var InputFile = function () {
    function InputFile() {}

    ;
    InputFile.value = new InputFile();
    return InputFile;
  }();

  var InputHidden = function () {
    function InputHidden() {}

    ;
    InputHidden.value = new InputHidden();
    return InputHidden;
  }();

  var InputImage = function () {
    function InputImage() {}

    ;
    InputImage.value = new InputImage();
    return InputImage;
  }();

  var InputMonth = function () {
    function InputMonth() {}

    ;
    InputMonth.value = new InputMonth();
    return InputMonth;
  }();

  var InputNumber = function () {
    function InputNumber() {}

    ;
    InputNumber.value = new InputNumber();
    return InputNumber;
  }();

  var InputPassword = function () {
    function InputPassword() {}

    ;
    InputPassword.value = new InputPassword();
    return InputPassword;
  }();

  var InputRadio = function () {
    function InputRadio() {}

    ;
    InputRadio.value = new InputRadio();
    return InputRadio;
  }();

  var InputRange = function () {
    function InputRange() {}

    ;
    InputRange.value = new InputRange();
    return InputRange;
  }();

  var InputReset = function () {
    function InputReset() {}

    ;
    InputReset.value = new InputReset();
    return InputReset;
  }();

  var InputSearch = function () {
    function InputSearch() {}

    ;
    InputSearch.value = new InputSearch();
    return InputSearch;
  }();

  var InputSubmit = function () {
    function InputSubmit() {}

    ;
    InputSubmit.value = new InputSubmit();
    return InputSubmit;
  }();

  var InputTel = function () {
    function InputTel() {}

    ;
    InputTel.value = new InputTel();
    return InputTel;
  }();

  var InputText = function () {
    function InputText() {}

    ;
    InputText.value = new InputText();
    return InputText;
  }();

  var InputTime = function () {
    function InputTime() {}

    ;
    InputTime.value = new InputTime();
    return InputTime;
  }();

  var InputUrl = function () {
    function InputUrl() {}

    ;
    InputUrl.value = new InputUrl();
    return InputUrl;
  }();

  var InputWeek = function () {
    function InputWeek() {}

    ;
    InputWeek.value = new InputWeek();
    return InputWeek;
  }();

  var renderInputType = function renderInputType(v) {
    if (v instanceof InputButton) {
      return "button";
    }

    ;

    if (v instanceof InputCheckbox) {
      return "checkbox";
    }

    ;

    if (v instanceof InputColor) {
      return "color";
    }

    ;

    if (v instanceof InputDate) {
      return "date";
    }

    ;

    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }

    ;

    if (v instanceof InputEmail) {
      return "email";
    }

    ;

    if (v instanceof InputFile) {
      return "file";
    }

    ;

    if (v instanceof InputHidden) {
      return "hidden";
    }

    ;

    if (v instanceof InputImage) {
      return "image";
    }

    ;

    if (v instanceof InputMonth) {
      return "month";
    }

    ;

    if (v instanceof InputNumber) {
      return "number";
    }

    ;

    if (v instanceof InputPassword) {
      return "password";
    }

    ;

    if (v instanceof InputRadio) {
      return "radio";
    }

    ;

    if (v instanceof InputRange) {
      return "range";
    }

    ;

    if (v instanceof InputReset) {
      return "reset";
    }

    ;

    if (v instanceof InputSearch) {
      return "search";
    }

    ;

    if (v instanceof InputSubmit) {
      return "submit";
    }

    ;

    if (v instanceof InputTel) {
      return "tel";
    }

    ;

    if (v instanceof InputText) {
      return "text";
    }

    ;

    if (v instanceof InputTime) {
      return "time";
    }

    ;

    if (v instanceof InputUrl) {
      return "url";
    }

    ;

    if (v instanceof InputWeek) {
      return "week";
    }

    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 28, column 19 - line 50, column 22): " + [v.constructor.name]);
  };

  exports["InputEmail"] = InputEmail;
  exports["InputPassword"] = InputPassword;
  exports["InputText"] = InputText;
  exports["renderInputType"] = renderInputType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query.Input"] = $PS["Halogen.Query.Input"] || {};
  var exports = $PS["Halogen.Query.Input"];
  var Data_Functor = $PS["Data.Functor"];

  var RefUpdate = function () {
    function RefUpdate(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    RefUpdate.create = function (value0) {
      return function (value1) {
        return new RefUpdate(value0, value1);
      };
    };

    return RefUpdate;
  }();

  var Action = function () {
    function Action(value0) {
      this.value0 = value0;
    }

    ;

    Action.create = function (value0) {
      return new Action(value0);
    };

    return Action;
  }();

  var functorInput = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof RefUpdate) {
        return new RefUpdate(m.value0, m.value1);
      }

      ;

      if (m instanceof Action) {
        return new Action(f(m.value0));
      }

      ;
      throw new Error("Failed pattern match at Halogen.Query.Input (line 19, column 1 - line 19, column 46): " + [m.constructor.name]);
    };
  });
  exports["RefUpdate"] = RefUpdate;
  exports["Action"] = Action;
  exports["functorInput"] = functorInput;
})(PS);

(function (exports) {
  "use strict";

  exports.eventListener = function (fn) {
    return function () {
      return function (event) {
        return fn(event)();
      };
    };
  };

  exports.addEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            return target.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  };

  exports.removeEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            return target.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  };
})(PS["Web.Event.EventTarget"] = PS["Web.Event.EventTarget"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.Event.EventTarget"] = $PS["Web.Event.EventTarget"] || {};
  var exports = $PS["Web.Event.EventTarget"];
  var $foreign = $PS["Web.Event.EventTarget"];
  exports["eventListener"] = $foreign.eventListener;
  exports["addEventListener"] = $foreign.addEventListener;
  exports["removeEventListener"] = $foreign.removeEventListener;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.DOM.Prop"] = $PS["Halogen.VDom.DOM.Prop"] || {};
  var exports = $PS["Halogen.VDom.DOM.Prop"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Foreign = $PS["Foreign"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];

  var Created = function () {
    function Created(value0) {
      this.value0 = value0;
    }

    ;

    Created.create = function (value0) {
      return new Created(value0);
    };

    return Created;
  }();

  var Removed = function () {
    function Removed(value0) {
      this.value0 = value0;
    }

    ;

    Removed.create = function (value0) {
      return new Removed(value0);
    };

    return Removed;
  }();

  var Attribute = function () {
    function Attribute(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Attribute.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Attribute(value0, value1, value2);
        };
      };
    };

    return Attribute;
  }();

  var Property = function () {
    function Property(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Property.create = function (value0) {
      return function (value1) {
        return new Property(value0, value1);
      };
    };

    return Property;
  }();

  var Handler = function () {
    function Handler(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Handler.create = function (value0) {
      return function (value1) {
        return new Handler(value0, value1);
      };
    };

    return Handler;
  }();

  var Ref = function () {
    function Ref(value0) {
      this.value0 = value0;
    }

    ;

    Ref.create = function (value0) {
      return new Ref(value0);
    };

    return Ref;
  }();

  var unsafeGetProperty = Halogen_VDom_Util.unsafeGetAny;
  var setProperty = Halogen_VDom_Util.unsafeSetAny;

  var removeProperty = function removeProperty(key, el) {
    var v = Foreign.typeOf(Halogen_VDom_Util.unsafeGetAny(key, el));

    if (v === "string") {
      return Halogen_VDom_Util.unsafeSetAny(key, "", el);
    }

    ;

    if (key === "rowSpan") {
      return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    }

    ;

    if (key === "colSpan") {
      return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    }

    ;
    return Halogen_VDom_Util.unsafeSetAny(key, Halogen_VDom_Util.jsUndefined, el);
  };

  var propToStrKey = function propToStrKey(v) {
    if (v instanceof Attribute && v.value0 instanceof Data_Maybe.Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }

    ;

    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }

    ;

    if (v instanceof Property) {
      return "prop/" + v.value0;
    }

    ;

    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }

    ;

    if (v instanceof Ref) {
      return "ref";
    }

    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };

  var propFromString = Unsafe_Coerce.unsafeCoerce;
  var propFromInt = Unsafe_Coerce.unsafeCoerce;
  var functorProp = new Data_Functor.Functor(function (f) {
    return function (v) {
      if (v instanceof Handler) {
        return new Handler(v.value0, Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v.value1));
      }

      ;

      if (v instanceof Ref) {
        return new Ref(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v.value0));
      }

      ;
      return v;
    };
  });

  var buildProp = function buildProp(emit) {
    return function (el) {
      var removeProp = function removeProp(prevEvents) {
        return function (v, v1) {
          if (v1 instanceof Attribute) {
            return Halogen_VDom_Util.removeAttribute(Data_Nullable.toNullable(v1.value0), v1.value1, el);
          }

          ;

          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }

          ;

          if (v1 instanceof Handler) {
            var handler = Halogen_VDom_Util.unsafeLookup(v1.value0, prevEvents);
            return Halogen_VDom_Util.removeEventListener(v1.value0, Data_Tuple.fst(handler), el);
          }

          ;

          if (v1 instanceof Ref) {
            return Data_Unit.unit;
          }

          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };

      var mbEmit = function mbEmit(v) {
        if (v instanceof Data_Maybe.Just) {
          return emit(v.value0)();
        }

        ;
        return Data_Unit.unit;
      };

      var haltProp = function haltProp(state) {
        var v = Foreign_Object.lookup("ref")(state.props);

        if (v instanceof Data_Maybe.Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }

        ;
        return Data_Unit.unit;
      };

      var diffProp = function diffProp(prevEvents, events) {
        return function (v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $61 = v11.value2 === v2.value2;

            if ($61) {
              return v2;
            }

            ;
            Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }

          ;

          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = Halogen_VDom_Util.refEq(v11.value1, v2.value1);

            if (v4) {
              return v2;
            }

            ;

            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $70 = Halogen_VDom_Util.refEq(elVal, v2.value1);

              if ($70) {
                return v2;
              }

              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }

            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }

          ;

          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler = Halogen_VDom_Util.unsafeLookup(v2.value0, prevEvents);
            Effect_Ref.write(v2.value1)(Data_Tuple.snd(handler))();
            Halogen_VDom_Util.pokeMutMap(v2.value0, handler, events);
            return v2;
          }

          ;
          return v2;
        };
      };

      var applyProp = function applyProp(events) {
        return function (v, v1, v2) {
          if (v2 instanceof Attribute) {
            Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }

          ;

          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }

          ;

          if (v2 instanceof Handler) {
            var v3 = Halogen_VDom_Util.unsafeGetAny(v2.value0, events);

            if (Halogen_VDom_Util.unsafeHasAny(v2.value0, events)) {
              Effect_Ref.write(v2.value1)(Data_Tuple.snd(v3))();
              return v2;
            }

            ;
            var v4 = Effect_Ref["new"](v2.value1)();
            var v5 = Web_Event_EventTarget.eventListener(function (ev) {
              return function __do() {
                var v5 = Effect_Ref.read(v4)();
                return mbEmit(v5(ev));
              };
            })();
            Halogen_VDom_Util.pokeMutMap(v2.value0, new Data_Tuple.Tuple(v5, v4), events);
            Halogen_VDom_Util.addEventListener(v2.value0, v5, el);
            return v2;
          }

          ;

          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }

          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };

      var patchProp = function patchProp(state, ps2) {
        var v = Halogen_VDom_Util.newMutMap();
        var onThis = removeProp(state.events);
        var onThese = diffProp(state.events, v);
        var onThat = applyProp(v);
        var v1 = Halogen_VDom_Util.diffWithKeyAndIxE(state.props, ps2, propToStrKey, onThese, onThis, onThat);
        var nextState = {
          events: Halogen_VDom_Util.unsafeFreeze(v),
          props: v1
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, nextState, patchProp, haltProp));
      };

      var renderProp = function renderProp(ps1) {
        var v = Halogen_VDom_Util.newMutMap();
        var v1 = Halogen_VDom_Util.strMapWithIxE(ps1, propToStrKey, applyProp(v));
        var state = {
          events: Halogen_VDom_Util.unsafeFreeze(v),
          props: v1
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, state, patchProp, haltProp));
      };

      return renderProp;
    };
  };

  exports["Attribute"] = Attribute;
  exports["Property"] = Property;
  exports["Handler"] = Handler;
  exports["propFromString"] = propFromString;
  exports["propFromInt"] = propFromInt;
  exports["buildProp"] = buildProp;
  exports["functorProp"] = functorProp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.HTML.Core"] = $PS["Halogen.HTML.Core"] || {};
  var exports = $PS["Halogen.HTML.Core"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var Halogen_VDom_DOM_Prop = $PS["Halogen.VDom.DOM.Prop"];
  var Halogen_VDom_Types = $PS["Halogen.VDom.Types"];

  var HTML = function HTML(x) {
    return x;
  };

  var ClassName = function ClassName(x) {
    return x;
  };

  var AttrName = function AttrName(x) {
    return x;
  };

  var IsProp = function IsProp(toPropValue) {
    this.toPropValue = toPropValue;
  };

  var widget = function widget($30) {
    return HTML(Halogen_VDom_Types.Widget.create($30));
  };

  var toPropValue = function toPropValue(dict) {
    return dict.toPropValue;
  };

  var text = function text($31) {
    return HTML(Halogen_VDom_Types.Text.create($31));
  };

  var prop = function prop(dictIsProp) {
    return function (v) {
      var $33 = Halogen_VDom_DOM_Prop.Property.create(v);
      var $34 = toPropValue(dictIsProp);
      return function ($35) {
        return $33($34($35));
      };
    };
  };

  var newtypeHTML = new Data_Newtype.Newtype(function (n) {
    return n;
  }, HTML);
  var newtypeClassName = new Data_Newtype.Newtype(function (n) {
    return n;
  }, ClassName);
  var isPropString = new IsProp(Halogen_VDom_DOM_Prop.propFromString);
  var isPropInt = new IsProp(Halogen_VDom_DOM_Prop.propFromInt);
  var isPropInputType = new IsProp(function ($49) {
    return Halogen_VDom_DOM_Prop.propFromString(DOM_HTML_Indexed_InputType.renderInputType($49));
  });
  var handler = Halogen_VDom_DOM_Prop.Handler.create;

  var element = function element(ns) {
    return function (name) {
      return function (props) {
        return function (children) {
          return new Halogen_VDom_Types.Elem(ns, name, props, children);
        };
      };
    };
  };

  var bifunctorHTML = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        return Data_Bifunctor.bimap(Halogen_VDom_Types.bifunctorVDom)(Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Halogen_VDom_DOM_Prop.functorProp)(Data_Functor.map(Halogen_Query_Input.functorInput)(g))))(f)(v);
      };
    };
  });

  var attr = function attr(ns) {
    return function (v) {
      return Halogen_VDom_DOM_Prop.Attribute.create(ns)(v);
    };
  };

  exports["widget"] = widget;
  exports["text"] = text;
  exports["element"] = element;
  exports["prop"] = prop;
  exports["attr"] = attr;
  exports["handler"] = handler;
  exports["AttrName"] = AttrName;
  exports["ClassName"] = ClassName;
  exports["newtypeHTML"] = newtypeHTML;
  exports["bifunctorHTML"] = bifunctorHTML;
  exports["isPropString"] = isPropString;
  exports["isPropInt"] = isPropInt;
  exports["isPropInputType"] = isPropInputType;
  exports["newtypeClassName"] = newtypeClassName;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.HTML"] = $PS["Halogen.HTML"] || {};
  var exports = $PS["Halogen.HTML"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];

  var slot = function slot(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (component) {
              return function (input) {
                return function (outputQuery) {
                  return Halogen_HTML_Core.widget(new Halogen_Component.ComponentSlot(Halogen_Component.componentSlot()(dictIsSymbol)(dictOrd)(label)(p)(component)(input)(outputQuery)));
                };
              };
            };
          };
        };
      };
    };
  };

  exports["slot"] = slot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Record"] = $PS["Record"] || {};
  var exports = $PS["Record"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Unsafe = $PS["Record.Unsafe"];

  var set = function set(dictIsSymbol) {
    return function (dictCons) {
      return function (dictCons1) {
        return function (l) {
          return function (b) {
            return function (r) {
              return Record_Unsafe.unsafeSet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(b)(r);
            };
          };
        };
      };
    };
  };

  var insert = function insert(dictIsSymbol) {
    return function (dictLacks) {
      return function (dictCons) {
        return function (l) {
          return function (a) {
            return function (r) {
              return Record_Unsafe.unsafeSet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(a)(r);
            };
          };
        };
      };
    };
  };

  var get = function get(dictIsSymbol) {
    return function (dictCons) {
      return function (l) {
        return function (r) {
          return Record_Unsafe.unsafeGet(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(r);
        };
      };
    };
  };

  exports["get"] = get;
  exports["set"] = set;
  exports["insert"] = insert;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Aff.Bus"] = $PS["Effect.Aff.Bus"] || {};
  var exports = $PS["Effect.Aff.Bus"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Effect_AVar = $PS["Effect.AVar"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_AVar = $PS["Effect.Aff.AVar"];
  var Effect_Class = $PS["Effect.Class"];

  var Bus = function () {
    function Bus(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Bus.create = function (value0) {
      return function (value1) {
        return new Bus(value0, value1);
      };
    };

    return Bus;
  }();

  var write = function write(a) {
    return function (v) {
      return Effect_Aff_AVar.put(a)(v.value0);
    };
  };

  var read = function read(v) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.empty)(function (v1) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.take(v.value1))(function (v2) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff_AVar.put(new Data_List_Types.Cons(v1, v2))(v.value1))(function () {
          return Effect_Aff_AVar.take(v1);
        });
      });
    });
  };

  var make = function make(dictMonadEffect) {
    return Effect_Class.liftEffect(dictMonadEffect)(function __do() {
      var v = Effect_AVar.empty();
      var v1 = Effect_AVar["new"](Data_Monoid.mempty(Data_List_Types.monoidList))();
      Effect_Aff.launchAff_(Effect_Aff.attempt(Control_Monad_Rec_Class.forever(Effect_Aff.monadRecAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.take(v))(function (v2) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.take(v1))(function (v3) {
          return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff_AVar.put(Data_List_Types.Nil.value)(v1))(function () {
            return Data_Foldable.sequence_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Data_Foldable.foldl(Data_List_Types.foldableList)(function (xs) {
              return function (a) {
                return new Data_List_Types.Cons(Effect_Aff_AVar.put(v2)(a), xs);
              };
            })(Data_Monoid.mempty(Data_List_Types.monoidList))(v3));
          });
        });
      }))))();
      return new Bus(v, v1);
    });
  };

  exports["make"] = make;
  exports["read"] = read;
  exports["write"] = write;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Component.Utils"] = $PS["Rtsv2App.Component.Utils"] || {};
  var exports = $PS["Rtsv2App.Component.Utils"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_Bus = $PS["Effect.Aff.Bus"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];

  var busEventSource = function busEventSource(dictMonadAff) {
    return function (bus) {
      return Halogen_Query_EventSource.affEventSource(dictMonadAff)(function (emitter) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff.forkAff(Control_Monad_Rec_Class.forever(Effect_Aff.monadRecAff)(Control_Bind.bindFlipped(Effect_Aff.bindAff)(Halogen_Query_EventSource.emit(emitter))(Effect_Aff_Bus.read(bus)))))(function (v) {
          return Control_Applicative.pure(Effect_Aff.applicativeAff)(Effect_Aff.killFiber(Effect_Exception.error("Event source closed"))(v));
        });
      });
    };
  };

  exports["busEventSource"] = busEventSource;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Component.HOC.Connect"] = $PS["Component.HOC.Connect"] || {};
  var exports = $PS["Component.HOC.Connect"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Record = $PS["Record"];
  var Rtsv2App_Component_Utils = $PS["Rtsv2App.Component.Utils"];

  var Initialize = function () {
    function Initialize() {}

    ;
    Initialize.value = new Initialize();
    return Initialize;
  }();

  var HandleUserBus = function () {
    function HandleUserBus(value0) {
      this.value0 = value0;
    }

    ;

    HandleUserBus.create = function (value0) {
      return new HandleUserBus(value0);
    };

    return HandleUserBus;
  }();

  var Emit = function () {
    function Emit(value0) {
      this.value0 = value0;
    }

    ;

    Emit.create = function (value0) {
      return new Emit(value0);
    };

    return Emit;
  }();

  var _inner = Data_Symbol.SProxy.value;

  var component = function component(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (dictLacks) {
        return function (innerComponent) {
          var render = function render(state) {
            return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "inner";
            }))(Data_Ord.ordUnit)(_inner)(Data_Unit.unit)(innerComponent)(state)(function ($24) {
              return Data_Maybe.Just.create(Emit.create($24));
            });
          };

          var handleQuery = Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
            return "inner";
          }))(Data_Ord.ordUnit)(_inner)(Data_Unit.unit);

          var handleAction = function handleAction(v) {
            if (v instanceof Initialize) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Reader_Class.asks(Halogen_Query_HalogenM.monadAskHalogenM(dictMonadAsk))(function (v1) {
                return v1.userEnv;
              }))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.subscribe(Data_Functor.map(Halogen_Query_EventSource.functorEventSource(dictMonadAff.MonadEffect0().Monad0().Bind1().Apply0().Functor0()))(HandleUserBus.create)(Rtsv2App_Component_Utils.busEventSource(dictMonadAff)(v1.userBus))))(function (v2) {
                  return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Effect_Ref.read(v1.currentUser)))(function (v3) {
                    return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v4) {
                      var $14 = {};

                      for (var $15 in v4) {
                        if ({}.hasOwnProperty.call(v4, $15)) {
                          $14[$15] = v4[$15];
                        }

                        ;
                      }

                      ;
                      $14.currentUser = v3;
                      return $14;
                    });
                  });
                });
              });
            }

            ;

            if (v instanceof HandleUserBus) {
              return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                var $19 = {};

                for (var $20 in v1) {
                  if ({}.hasOwnProperty.call(v1, $20)) {
                    $19[$20] = v1[$20];
                  }

                  ;
                }

                ;
                $19.currentUser = v.value0;
                return $19;
              });
            }

            ;

            if (v instanceof Emit) {
              return Halogen_Query_HalogenM.raise(v.value0);
            }

            ;
            throw new Error("Failed pattern match at Component.HOC.Connect (line 50, column 18 - line 61, column 21): " + [v.constructor.name]);
          };

          return Halogen_Component.mkComponent({
            initialState: Record.insert(new Data_Symbol.IsSymbol(function () {
              return "currentUser";
            }))()()(Data_Symbol.SProxy.value)(Data_Maybe.Nothing.value),
            render: render,
            "eval": Halogen_Component.mkEval({
              handleAction: handleAction,
              handleQuery: handleQuery,
              receive: Halogen_Component.defaultEval.receive,
              initialize: new Data_Maybe.Just(Initialize.value),
              finalize: Halogen_Component.defaultEval.finalize
            })
          });
        };
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Alternative"] = $PS["Control.Alternative"] || {};
  var exports = $PS["Control.Alternative"];

  var Alternative = function Alternative(Applicative0, Plus1) {
    this.Applicative0 = Applicative0;
    this.Plus1 = Plus1;
  };

  exports["Alternative"] = Alternative;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.Fork.Class"] = $PS["Control.Monad.Fork.Class"] || {};
  var exports = $PS["Control.Monad.Fork.Class"];
  var Effect_Aff = $PS["Effect.Aff"];

  var MonadFork = function MonadFork(Functor1, Monad0, fork, join, suspend) {
    this.Functor1 = Functor1;
    this.Monad0 = Monad0;
    this.fork = fork;
    this.join = join;
    this.suspend = suspend;
  };

  var monadForkAff = new MonadFork(function () {
    return Effect_Aff.functorFiber;
  }, function () {
    return Effect_Aff.monadAff;
  }, Effect_Aff.forkAff, Effect_Aff.joinFiber, Effect_Aff.suspendAff);

  var fork = function fork(dict) {
    return dict.fork;
  };

  exports["fork"] = fork;
  exports["monadForkAff"] = monadForkAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Monad.State.Trans"] = $PS["Control.Monad.State.Trans"] || {};
  var exports = $PS["Control.Monad.State.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];

  var StateT = function StateT(x) {
    return x;
  };

  var runStateT = function runStateT(v) {
    return v;
  };

  var lazyStateT = new Control_Lazy.Lazy(function (f) {
    return function (s) {
      var v = f(Data_Unit.unit);
      return v(s);
    };
  });

  var functorStateT = function functorStateT(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return function (v) {
        return function (s) {
          return Data_Functor.map(dictFunctor)(function (v1) {
            return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    });
  };

  var evalStateT = function evalStateT(dictFunctor) {
    return function (v) {
      return function (s) {
        return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
      };
    };
  };

  var monadStateT = function monadStateT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeStateT(dictMonad);
    }, function () {
      return bindStateT(dictMonad);
    });
  };

  var bindStateT = function bindStateT(dictMonad) {
    return new Control_Bind.Bind(function () {
      return applyStateT(dictMonad);
    }, function (v) {
      return function (f) {
        return function (s) {
          return Control_Bind.bind(dictMonad.Bind1())(v(s))(function (v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    });
  };

  var applyStateT = function applyStateT(dictMonad) {
    return new Control_Apply.Apply(function () {
      return functorStateT(dictMonad.Bind1().Apply0().Functor0());
    }, Control_Monad.ap(monadStateT(dictMonad)));
  };

  var applicativeStateT = function applicativeStateT(dictMonad) {
    return new Control_Applicative.Applicative(function () {
      return applyStateT(dictMonad);
    }, function (a) {
      return function (s) {
        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, s));
      };
    });
  };

  var monadStateStateT = function monadStateStateT(dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
      return monadStateT(dictMonad);
    }, function (f) {
      return StateT(function () {
        var $114 = Control_Applicative.pure(dictMonad.Applicative0());
        return function ($115) {
          return $114(f($115));
        };
      }());
    });
  };

  exports["StateT"] = StateT;
  exports["runStateT"] = runStateT;
  exports["evalStateT"] = evalStateT;
  exports["functorStateT"] = functorStateT;
  exports["monadStateT"] = monadStateT;
  exports["lazyStateT"] = lazyStateT;
  exports["monadStateStateT"] = monadStateStateT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Control.Parallel"] = $PS["Control.Parallel"] || {};
  var exports = $PS["Control.Parallel"];
  var Control_Category = $PS["Control.Category"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Foldable = $PS["Data.Foldable"];

  var parTraverse_ = function parTraverse_(dictParallel) {
    return function (dictFoldable) {
      return function (f) {
        var $17 = Control_Parallel_Class.sequential(dictParallel);
        var $18 = Data_Foldable.traverse_(dictParallel.Applicative1())(dictFoldable)(function () {
          var $20 = Control_Parallel_Class.parallel(dictParallel);
          return function ($21) {
            return $20(f($21));
          };
        }());
        return function ($19) {
          return $17($18($19));
        };
      };
    };
  };

  var parSequence_ = function parSequence_(dictParallel) {
    return function (dictFoldable) {
      return parTraverse_(dictParallel)(dictFoldable)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  exports["parSequence_"] = parSequence_;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Class"] = $PS["Data.Argonaut.Decode.Class"] || {};
  var exports = $PS["Data.Argonaut.Decode.Class"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Foreign_Object = $PS["Foreign.Object"];

  var DecodeJson = function DecodeJson(decodeJson) {
    this.decodeJson = decodeJson;
  };

  var decodeJsonString = new DecodeJson(Data_Argonaut_Core.caseJsonString(new Data_Either.Left("Value is not a String"))(Data_Either.Right.create));
  var decodeJsonJson = new DecodeJson(Data_Either.Right.create);
  var decodeJsonBoolean = new DecodeJson(Data_Argonaut_Core.caseJsonBoolean(new Data_Either.Left("Value is not a Boolean"))(Data_Either.Right.create));

  var decodeJson = function decodeJson(dict) {
    return dict.decodeJson;
  };

  var decodeJsonMaybe = function decodeJsonMaybe(dictDecodeJson) {
    return new DecodeJson(function (j) {
      if (Data_Argonaut_Core.isNull(j)) {
        return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
      }

      ;

      if (Data_Boolean.otherwise) {
        return Data_Functor.map(Data_Either.functorEither)(Data_Maybe.Just.create)(decodeJson(dictDecodeJson)(j));
      }

      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 30, column 1 - line 33, column 40): " + [j.constructor.name]);
    });
  };

  var decodeJObject = function () {
    var $64 = Data_Maybe.maybe(new Data_Either.Left("Value is not an Object"))(Data_Either.Right.create);
    return function ($65) {
      return $64(Data_Argonaut_Core.toObject($65));
    };
  }();

  var decodeForeignObject = function decodeForeignObject(dictDecodeJson) {
    return new DecodeJson(function () {
      var $91 = Data_Bifunctor.lmap(Data_Either.bifunctorEither)(function (v) {
        return "Couldn't decode ForeignObject: " + v;
      });
      var $92 = Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(Data_Traversable.traverse(Foreign_Object.traversableObject)(Data_Either.applicativeEither)(decodeJson(dictDecodeJson)))(decodeJObject);
      return function ($93) {
        return $91($92($93));
      };
    }());
  };

  exports["decodeJson"] = decodeJson;
  exports["DecodeJson"] = DecodeJson;
  exports["decodeJsonMaybe"] = decodeJsonMaybe;
  exports["decodeJsonBoolean"] = decodeJsonBoolean;
  exports["decodeJsonString"] = decodeJsonString;
  exports["decodeJsonJson"] = decodeJsonJson;
  exports["decodeForeignObject"] = decodeForeignObject;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Operator.Bottom"] = $PS["Data.Operator.Bottom"] || {};
  var exports = $PS["Data.Operator.Bottom"];
  var Data_Either = $PS["Data.Either"];

  var Bottom2 = function Bottom2(bottom2) {
    this.bottom2 = bottom2;
  };

  var bottom2Either = new Bottom2(Data_Either.Left.create);

  var bottom2 = function bottom2(dict) {
    return dict.bottom2;
  };

  exports["bottom2"] = bottom2;
  exports["bottom2Either"] = bottom2Either;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Utils"] = $PS["Data.Argonaut.Decode.Struct.Utils"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Bottom = $PS["Data.Operator.Bottom"];
  var notObjectErrorMessage = "Could not convert JSON to object";

  var reportJson = function reportJson(dictBottom2) {
    return function (f) {
      return function (json) {
        var v = Data_Argonaut_Core.toObject(json);

        if (v instanceof Data_Maybe.Just) {
          return f(v.value0);
        }

        ;

        if (v instanceof Data_Maybe.Nothing) {
          return Data_Operator_Bottom.bottom2(dictBottom2)(notObjectErrorMessage);
        }

        ;
        throw new Error("Failed pattern match at Data.Argonaut.Decode.Struct.Utils (line 40, column 3 - line 42, column 45): " + [v.constructor.name]);
      };
    };
  };

  var getMissingFieldErrorMessage = function getMissingFieldErrorMessage(fieldName) {
    return "JSON was missing expected field: " + fieldName;
  };

  var elaborateFailure = function elaborateFailure(s) {
    return function (e) {
      var msg = function msg(m) {
        return "Failed to decode key '" + (s + ("': " + m));
      };

      return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(msg)(e);
    };
  };

  exports["elaborateFailure"] = elaborateFailure;
  exports["getMissingFieldErrorMessage"] = getMissingFieldErrorMessage;
  exports["notObjectErrorMessage"] = notObjectErrorMessage;
  exports["reportJson"] = reportJson;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Operator.Top"] = $PS["Data.Operator.Top"] || {};
  var exports = $PS["Data.Operator.Top"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Either = $PS["Data.Either"];

  var Top1_ = function Top1_(top1_) {
    this.top1_ = top1_;
  };

  var top1_Either = new Top1_(Control_Applicative.pure(Data_Either.applicativeEither));

  var top1_ = function top1_(dict) {
    return dict.top1_;
  };

  exports["top1_"] = top1_;
  exports["top1_Either"] = top1_Either;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Struct.Get.RGet"] = $PS["Data.Struct.Get.RGet"] || {};
  var exports = $PS["Data.Struct.Get.RGet"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record = $PS["Record"];

  var RGet = function RGet(rget) {
    this.rget = rget;
  };

  var rgetRecord = function rgetRecord(dictIsSymbol) {
    return function (dictSProxying) {
      return new RGet(function (dictCons) {
        return function (v) {
          return function (v1) {
            return Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value);
          };
        };
      });
    };
  };

  var rget = function rget(dict) {
    return dict.rget;
  };

  exports["rget"] = rget;
  exports["rgetRecord"] = rgetRecord;
})(PS);

(function (exports) {
  "use strict";

  exports.copyRecord = function (rec) {
    var copy = {};

    for (var key in rec) {
      if ({}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }

    return copy;
  };

  exports.unsafeInsert = function (l) {
    return function (a) {
      return function (rec) {
        rec[l] = a;
        return rec;
      };
    };
  };

  exports.unsafeModify = function (l) {
    return function (f) {
      return function (rec) {
        rec[l] = f(rec[l]);
        return rec;
      };
    };
  };

  exports.unsafeDelete = function (l) {
    return function (rec) {
      delete rec[l];
      return rec;
    };
  };
})(PS["Record.Builder"] = PS["Record.Builder"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Record.Builder"] = $PS["Record.Builder"] || {};
  var exports = $PS["Record.Builder"];
  var $foreign = $PS["Record.Builder"];
  var Control_Category = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Symbol = $PS["Data.Symbol"];
  var semigroupoidBuilder = Control_Semigroupoid.semigroupoidFn;

  var modify = function modify(dictCons) {
    return function (dictCons1) {
      return function (dictIsSymbol) {
        return function (l) {
          return function (f) {
            return function (r1) {
              return $foreign.unsafeModify(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(f)(r1);
            };
          };
        };
      };
    };
  };

  var insert = function insert(dictCons) {
    return function (dictLacks) {
      return function (dictIsSymbol) {
        return function (l) {
          return function (a) {
            return function (r1) {
              return $foreign.unsafeInsert(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(a)(r1);
            };
          };
        };
      };
    };
  };

  var $$delete = function $$delete(dictIsSymbol) {
    return function (dictLacks) {
      return function (dictCons) {
        return function (l) {
          return function (r2) {
            return $foreign.unsafeDelete(Data_Symbol.reflectSymbol(dictIsSymbol)(l))(r2);
          };
        };
      };
    };
  };

  var categoryBuilder = Control_Category.categoryFn;

  var build = function build(v) {
    return function (r1) {
      return v($foreign.copyRecord(r1));
    };
  };

  exports["build"] = build;
  exports["insert"] = insert;
  exports["modify"] = modify;
  exports["delete"] = $$delete;
  exports["semigroupoidBuilder"] = semigroupoidBuilder;
  exports["categoryBuilder"] = categoryBuilder;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Struct.Insert.RInsert"] = $PS["Data.Struct.Insert.RInsert"] || {};
  var exports = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Builder = $PS["Record.Builder"];

  var RInsert = function RInsert(rinsert) {
    this.rinsert = rinsert;
  };

  var rinsertBuilder = function rinsertBuilder(dictIsSymbol) {
    return function (dictSProxying) {
      return new RInsert(function (dictCons) {
        return function (dictLacks) {
          return function (v) {
            return function (v1) {
              return function (v2) {
                return Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value);
              };
            };
          };
        };
      });
    };
  };

  var rinsert = function rinsert(dict) {
    return dict.rinsert;
  };

  exports["rinsert"] = rinsert;
  exports["rinsertBuilder"] = rinsertBuilder;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Type.Equality"] = $PS["Type.Equality"] || {};
  var exports = $PS["Type.Equality"];

  var TypeEquals = function TypeEquals(from, to) {
    this.from = from;
    this.to = to;
  };

  var to = function to(dict) {
    return dict.to;
  };

  var refl = new TypeEquals(function (a) {
    return a;
  }, function (a) {
    return a;
  });

  var from = function from(dict) {
    return dict.from;
  };

  exports["to"] = to;
  exports["from"] = from;
  exports["refl"] = refl;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith"] = $PS["Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Argonaut_Decode_Struct_Utils = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Bottom = $PS["Data.Operator.Bottom"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Get_RGet = $PS["Data.Struct.Get.RGet"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];
  var Type_Equality = $PS["Type.Equality"];

  var DecodeJsonWith = function DecodeJsonWith(decodeJsonWith) {
    this.decodeJsonWith = decodeJsonWith;
  };

  var decodeJsonWithNil = function decodeJsonWithNil(dictCategory) {
    return function (dictTop1_) {
      return new DecodeJsonWith(function (v) {
        return function (v1) {
          return function (v2) {
            return function (v3) {
              return function (v4) {
                return Data_Operator_Top.top1_(dictTop1_)(Control_Category.identity(dictCategory));
              };
            };
          };
        };
      });
    };
  };

  var decodeJsonWith = function decodeJsonWith(dict) {
    return dict.decodeJsonWith;
  };

  var decodeJsonWithCons = function decodeJsonWithCons(dictBind) {
    return function (dictBottom2) {
      return function (dictCons) {
        return function (dictCons1) {
          return function (dictDecodeJsonWith) {
            return function (dictIsSymbol) {
              return function (dictLacks) {
                return function (dictRGet) {
                  return function (dictRInsert) {
                    return function (dictSemigroupoid) {
                      return function (dictTop1_) {
                        return function (dictTypeEquals) {
                          return new DecodeJsonWith(function (v) {
                            return function (v1) {
                              return function (decoderStruct) {
                                return function (object) {
                                  return function (x) {
                                    var fieldName = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                                    var decoder = Type_Equality.to(dictTypeEquals)(Data_Struct_Get_RGet.rget(dictRGet)()(Type_Data_RowList.RLProxy.value)(Data_Symbol.SProxy.value)(decoderStruct));
                                    var v2 = Foreign_Object.lookup(fieldName)(object);

                                    if (v2 instanceof Data_Maybe.Just) {
                                      return Control_Bind.bind(dictBind)(decoder(v2.value0)(x))(function (v3) {
                                        return Control_Bind.bind(dictBind)(decodeJsonWith(dictDecodeJsonWith)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(decoderStruct)(object)(x))(function (v4) {
                                          return Data_Operator_Top.top1_(dictTop1_)(Control_Semigroupoid.compose(dictSemigroupoid)(Data_Struct_Insert_RInsert.rinsert(dictRInsert)()()(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(Data_Symbol.SProxy.value)(v3))(v4));
                                        });
                                      });
                                    }

                                    ;

                                    if (v2 instanceof Data_Maybe.Nothing) {
                                      return Data_Operator_Bottom.bottom2(dictBottom2)(Data_Argonaut_Decode_Struct_Utils.getMissingFieldErrorMessage(fieldName));
                                    }

                                    ;
                                    throw new Error("Failed pattern match at Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith (line 77, column 5 - line 83, column 56): " + [v2.constructor.name]);
                                  };
                                };
                              };
                            };
                          });
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  exports["decodeJsonWith"] = decodeJsonWith;
  exports["decodeJsonWithNil"] = decodeJsonWithNil;
  exports["decodeJsonWithCons"] = decodeJsonWithCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"] = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Plus = $PS["Control.Plus"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Utils = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Bottom = $PS["Data.Operator.Bottom"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var GDecodeJson = function GDecodeJson(gDecodeJson) {
    this.gDecodeJson = gDecodeJson;
  };

  var gDecodeJson_NilNilNil = function gDecodeJson_NilNilNil(dictCategory) {
    return function (dictTop1_) {
      return new GDecodeJson(function (v) {
        return function (v1) {
          return function (v2) {
            return Data_Operator_Top.top1_(dictTop1_)(Control_Category.identity(dictCategory));
          };
        };
      });
    };
  };

  var gDecodeJson = function gDecodeJson(dict) {
    return dict.gDecodeJson;
  };

  var gDecodeJson_ConsNilCons_Plus = function gDecodeJson_ConsNilCons_Plus(dictCons) {
    return function (dictDecodeJson) {
      return function (dictGDecodeJson) {
        return function (dictIsSymbol) {
          return function (dictLacks) {
            return function (dictPlus) {
              return function (dictRInsert) {
                return function (dictSemigroupoid) {
                  return new GDecodeJson(function (v) {
                    return function (v1) {
                      return function (object) {
                        var fieldName = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        return Control_Bind.bind(Data_Either.bindEither)(gDecodeJson(dictGDecodeJson)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(object))(function (v2) {
                          var v3 = Foreign_Object.lookup(fieldName)(object);

                          if (v3 instanceof Data_Maybe.Just) {
                            return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson)(v3.value0))(function (v4) {
                              return Data_Operator_Top.top1_(Data_Operator_Top.top1_Either)(Control_Semigroupoid.compose(dictSemigroupoid)(Data_Struct_Insert_RInsert.rinsert(dictRInsert)()()(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(Data_Symbol.SProxy.value)(v4))(v2));
                            });
                          }

                          ;

                          if (v3 instanceof Data_Maybe.Nothing) {
                            return Data_Operator_Top.top1_(Data_Operator_Top.top1_Either)(Control_Semigroupoid.compose(dictSemigroupoid)(Data_Struct_Insert_RInsert.rinsert(dictRInsert)()()(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(Data_Symbol.SProxy.value)(Control_Plus.empty(dictPlus)))(v2));
                          }

                          ;
                          throw new Error("Failed pattern match at Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson (line 62, column 5 - line 67, column 57): " + [v3.constructor.name]);
                        });
                      };
                    };
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  var gDecodeJson_ConsNilCons_nonPlus = function gDecodeJson_ConsNilCons_nonPlus(dictCons) {
    return function (dictDecodeJson) {
      return function (dictGDecodeJson) {
        return function (dictIsSymbol) {
          return function (dictLacks) {
            return function (dictRInsert) {
              return function (dictSemigroupoid) {
                return new GDecodeJson(function (v) {
                  return function (v1) {
                    return function (object) {
                      var fieldName = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                      var v2 = Foreign_Object.lookup(fieldName)(object);

                      if (v2 instanceof Data_Maybe.Just) {
                        return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson)(v2.value0))(function (v3) {
                          return Control_Bind.bind(Data_Either.bindEither)(gDecodeJson(dictGDecodeJson)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(object))(function (v4) {
                            return Data_Operator_Top.top1_(Data_Operator_Top.top1_Either)(Control_Semigroupoid.compose(dictSemigroupoid)(Data_Struct_Insert_RInsert.rinsert(dictRInsert)()()(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(Data_Symbol.SProxy.value)(v3))(v4));
                          });
                        });
                      }

                      ;

                      if (v2 instanceof Data_Maybe.Nothing) {
                        return Data_Operator_Bottom.bottom2(Data_Operator_Bottom.bottom2Either)(Data_Argonaut_Decode_Struct_Utils.getMissingFieldErrorMessage(fieldName));
                      }

                      ;
                      throw new Error("Failed pattern match at Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson (line 89, column 5 - line 95, column 56): " + [v2.constructor.name]);
                    };
                  };
                });
              };
            };
          };
        };
      };
    };
  };

  exports["gDecodeJson"] = gDecodeJson;
  exports["gDecodeJson_NilNilNil"] = gDecodeJson_NilNilNil;
  exports["gDecodeJson_ConsNilCons_Plus"] = gDecodeJson_ConsNilCons_Plus;
  exports["gDecodeJson_ConsNilCons_nonPlus"] = gDecodeJson_ConsNilCons_nonPlus;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"] = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Argonaut_Decode_Struct_Utils = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Bottom = $PS["Data.Operator.Bottom"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Record_Builder = $PS["Record.Builder"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var DecodeJson = function DecodeJson(decodeJson) {
    this.decodeJson = decodeJson;
  };

  var decodeJsonRecord = function decodeJsonRecord(dictGDecodeJson) {
    return function (dictRowToList) {
      return new DecodeJson(function (json) {
        var v = Data_Argonaut_Core.toObject(json);

        if (v instanceof Data_Maybe.Just) {
          return Control_Bind.bind(Data_Either.bindEither)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson(dictGDecodeJson)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(v.value0))(function (v1) {
            return Data_Operator_Top.top1_(Data_Operator_Top.top1_Either)(Record_Builder.build(v1)({}));
          });
        }

        ;

        if (v instanceof Data_Maybe.Nothing) {
          return Data_Operator_Bottom.bottom2(Data_Operator_Bottom.bottom2Either)(Data_Argonaut_Decode_Struct_Utils.notObjectErrorMessage);
        }

        ;
        throw new Error("Failed pattern match at Data.Argonaut.Decode.Struct.Tolerant.DecodeJson (line 32, column 5 - line 41, column 38): " + [v.constructor.name]);
      });
    };
  };

  var decodeJson = function decodeJson(dict) {
    return dict.decodeJson;
  };

  var decodeDecodeJson = function decodeDecodeJson(dictDecodeJson) {
    return new DecodeJson(Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson));
  };

  exports["decodeJson"] = decodeJson;
  exports["decodeJsonRecord"] = decodeJsonRecord;
  exports["decodeDecodeJson"] = decodeDecodeJson;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Tolerant.Combinators"] = $PS["Data.Argonaut.Decode.Struct.Tolerant.Combinators"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Tolerant.Combinators"];
  var Data_Argonaut_Decode_Struct_Tolerant_DecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Data_Argonaut_Decode_Struct_Utils = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];
  var Foreign_Object = $PS["Foreign.Object"];

  var getField = function getField(dictDecodeJson) {
    return function (o) {
      return function (s) {
        return Data_Maybe.maybe(Data_Either.Left.create("Expected field " + Data_Show.show(Data_Show.showString)(s)))(function () {
          var $4 = Data_Argonaut_Decode_Struct_Utils.elaborateFailure(s);
          var $5 = Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeJson(dictDecodeJson);
          return function ($6) {
            return $4($5($6));
          };
        }())(Foreign_Object.lookup(s)(o));
      };
    };
  };

  exports["getField"] = getField;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils"] = $PS["Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils"] || {};
  var exports = $PS["Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Decode_Struct_Cross_DecodeJsonWith = $PS["Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Argonaut_Decode_Struct_Utils = $PS["Data.Argonaut.Decode.Struct.Utils"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Record_Builder = $PS["Record.Builder"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var decodeJsonWith = function decodeJsonWith(dictBind) {
    return function (dictBottom2) {
      return function (dictDecodeJsonWith) {
        return function (dictGDecodeJson) {
          return function (dictRowToList) {
            return function (dictRowToList1) {
              return function (dictTop1_) {
                return function (decoderRecord) {
                  var go = function go(object) {
                    return Control_Bind.bind(dictBind)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson(dictGDecodeJson)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(object))(function (v) {
                      var record2 = Record_Builder.build(v)({});
                      return Control_Bind.bind(dictBind)(Data_Argonaut_Decode_Struct_Cross_DecodeJsonWith.decodeJsonWith(dictDecodeJsonWith)(Type_Data_RowList.RLProxy.value)(Type_Data_RowList.RLProxy.value)(decoderRecord)(object)(record2))(function (v1) {
                        return Data_Operator_Top.top1_(dictTop1_)(Record_Builder.build(v1)(record2));
                      });
                    });
                  };

                  return Data_Argonaut_Decode_Struct_Utils.reportJson(dictBottom2)(go);
                };
              };
            };
          };
        };
      };
    };
  };

  exports["decodeJsonWith"] = decodeJsonWith;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Argonaut.Encode.Class"] = $PS["Data.Argonaut.Encode.Class"] || {};
  var exports = $PS["Data.Argonaut.Encode.Class"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Record = $PS["Record"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var GEncodeJson = function GEncodeJson(gEncodeJson) {
    this.gEncodeJson = gEncodeJson;
  };

  var EncodeJson = function EncodeJson(encodeJson) {
    this.encodeJson = encodeJson;
  };

  var gEncodeJsonNil = new GEncodeJson(function (v) {
    return function (v1) {
      return Foreign_Object.empty;
    };
  });

  var gEncodeJson = function gEncodeJson(dict) {
    return dict.gEncodeJson;
  };

  var encodeRecord = function encodeRecord(dictGEncodeJson) {
    return function (dictRowToList) {
      return new EncodeJson(function (rec) {
        return Data_Argonaut_Core.fromObject(gEncodeJson(dictGEncodeJson)(rec)(Type_Data_RowList.RLProxy.value));
      });
    };
  };

  var encodeJsonJString = new EncodeJson(Data_Argonaut_Core.fromString);

  var encodeJson = function encodeJson(dict) {
    return dict.encodeJson;
  };

  var encodeJsonMaybe = function encodeJsonMaybe(dictEncodeJson) {
    return new EncodeJson(function (v) {
      if (v instanceof Data_Maybe.Nothing) {
        return Data_Argonaut_Core.jsonNull;
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return encodeJson(dictEncodeJson)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Argonaut.Encode.Class (line 29, column 1 - line 31, column 37): " + [v.constructor.name]);
    });
  };

  var gEncodeJsonCons = function gEncodeJsonCons(dictEncodeJson) {
    return function (dictGEncodeJson) {
      return function (dictIsSymbol) {
        return function (dictCons) {
          return new GEncodeJson(function (row) {
            return function (v) {
              return Foreign_Object.insert(Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value))(encodeJson(dictEncodeJson)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(row)))(gEncodeJson(dictGEncodeJson)(row)(Type_Data_RowList.RLProxy.value));
            };
          });
        };
      };
    };
  };

  exports["encodeJson"] = encodeJson;
  exports["encodeJsonMaybe"] = encodeJsonMaybe;
  exports["encodeJsonJString"] = encodeJsonJString;
  exports["encodeRecord"] = encodeRecord;
  exports["gEncodeJsonNil"] = gEncodeJsonNil;
  exports["gEncodeJsonCons"] = gEncodeJsonCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Array.NonEmpty"] = $PS["Data.Array.NonEmpty"] || {};
  var exports = $PS["Data.Array.NonEmpty"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var unsafeFromArray = Unsafe_Coerce.unsafeCoerce;
  var toArray = Unsafe_Coerce.unsafeCoerce;

  var snoc$prime = function snoc$prime(xs) {
    return function (x) {
      return unsafeFromArray(Data_Array.snoc(xs)(x));
    };
  };

  var snoc = function snoc(xs) {
    return function (x) {
      return unsafeFromArray(Data_Array.snoc(toArray(xs))(x));
    };
  };

  var singleton = function singleton($49) {
    return unsafeFromArray(Data_Array.singleton($49));
  };

  var fromArray = function fromArray(xs) {
    if (Data_Array.length(xs) > 0) {
      return new Data_Maybe.Just(unsafeFromArray(xs));
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Array.NonEmpty (line 134, column 1 - line 134, column 58): " + [xs.constructor.name]);
  };

  var cons$prime = function cons$prime(x) {
    return function (xs) {
      return unsafeFromArray(Data_Array.cons(x)(xs));
    };
  };

  var adaptMaybe = function adaptMaybe(f) {
    var $64 = Data_Maybe.fromJust();
    return function ($65) {
      return $64(f(toArray($65)));
    };
  };

  var head = adaptMaybe(Data_Array.head);
  var init = adaptMaybe(Data_Array.init);
  var last = adaptMaybe(Data_Array.last);
  var tail = adaptMaybe(Data_Array.tail);

  var adaptAny = function adaptAny(f) {
    return function ($67) {
      return f(toArray($67));
    };
  };

  var length = adaptAny(Data_Array.length);

  var unsafeAdapt = function unsafeAdapt(f) {
    var $68 = adaptAny(f);
    return function ($69) {
      return unsafeFromArray($68($69));
    };
  };

  var cons = function cons(x) {
    return unsafeAdapt(Data_Array.cons(x));
  };

  exports["fromArray"] = fromArray;
  exports["singleton"] = singleton;
  exports["cons"] = cons;
  exports["cons'"] = cons$prime;
  exports["snoc"] = snoc;
  exports["snoc'"] = snoc$prime;
  exports["head"] = head;
  exports["last"] = last;
  exports["tail"] = tail;
  exports["init"] = init;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Array.NonEmpty.Internal"] = $PS["Data.Array.NonEmpty.Internal"] || {};
  var exports = $PS["Data.Array.NonEmpty.Internal"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var semigroupNonEmptyArray = Data_Semigroup.semigroupArray;
  var functorNonEmptyArray = Data_Functor.functorArray;
  var foldableNonEmptyArray = Data_Foldable.foldableArray;
  exports["semigroupNonEmptyArray"] = semigroupNonEmptyArray;
  exports["functorNonEmptyArray"] = functorNonEmptyArray;
  exports["foldableNonEmptyArray"] = foldableNonEmptyArray;
})(PS);

(function (exports) {
  "use strict";

  exports.topInt = 2147483647;
  exports.bottomInt = -2147483648;
  exports.topChar = String.fromCharCode(65535);
  exports.bottomChar = String.fromCharCode(0);
})(PS["Data.Bounded"] = PS["Data.Bounded"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Bounded"] = $PS["Data.Bounded"] || {};
  var exports = $PS["Data.Bounded"];
  var $foreign = $PS["Data.Bounded"];
  var Data_Ord = $PS["Data.Ord"];

  var Bounded = function Bounded(Ord0, bottom, top) {
    this.Ord0 = Ord0;
    this.bottom = bottom;
    this.top = top;
  };

  var top = function top(dict) {
    return dict.top;
  };

  var boundedInt = new Bounded(function () {
    return Data_Ord.ordInt;
  }, $foreign.bottomInt, $foreign.topInt);
  var boundedChar = new Bounded(function () {
    return Data_Ord.ordChar;
  }, $foreign.bottomChar, $foreign.topChar);

  var bottom = function bottom(dict) {
    return dict.bottom;
  };

  exports["Bounded"] = Bounded;
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["boundedInt"] = boundedInt;
  exports["boundedChar"] = boundedChar;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.CommutativeRing"] = $PS["Data.CommutativeRing"] || {};
  var exports = $PS["Data.CommutativeRing"];
  var Data_Ring = $PS["Data.Ring"];

  var CommutativeRing = function CommutativeRing(Ring0) {
    this.Ring0 = Ring0;
  };

  var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
  });
  exports["commutativeRingInt"] = commutativeRingInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Const"] = $PS["Data.Const"] || {};
  var exports = $PS["Data.Const"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Const = function Const(x) {
    return x;
  };

  var newtypeConst = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Const);
  var functorConst = new Data_Functor.Functor(function (f) {
    return function (m) {
      return m;
    };
  });

  var applyConst = function applyConst(dictSemigroup) {
    return new Control_Apply.Apply(function () {
      return functorConst;
    }, function (v) {
      return function (v1) {
        return Data_Semigroup.append(dictSemigroup)(v)(v1);
      };
    });
  };

  var applicativeConst = function applicativeConst(dictMonoid) {
    return new Control_Applicative.Applicative(function () {
      return applyConst(dictMonoid.Semigroup0());
    }, function (v) {
      return Data_Monoid.mempty(dictMonoid);
    });
  };

  exports["Const"] = Const;
  exports["newtypeConst"] = newtypeConst;
  exports["applicativeConst"] = applicativeConst;
})(PS);

(function (exports) {
  "use strict";

  var createDate = function createDate(y, m, d) {
    var date = new Date(Date.UTC(y, m, d));

    if (y >= 0 && y < 100) {
      date.setUTCFullYear(y);
    }

    return date;
  };

  exports.canonicalDateImpl = function (ctor, y, m, d) {
    var date = createDate(y, m - 1, d);
    return ctor(date.getUTCFullYear())(date.getUTCMonth() + 1)(date.getUTCDate());
  };

  exports.calcWeekday = function (y, m, d) {
    return createDate(y, m - 1, d).getUTCDay();
  };
})(PS["Data.Date"] = PS["Data.Date"] || {});

(function (exports) {
  "use strict";

  exports.toCharCode = function (c) {
    return c.charCodeAt(0);
  };

  exports.fromCharCode = function (c) {
    return String.fromCharCode(c);
  };
})(PS["Data.Enum"] = PS["Data.Enum"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Enum"] = $PS["Data.Enum"] || {};
  var exports = $PS["Data.Enum"];
  var $foreign = $PS["Data.Enum"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];

  var Enum = function Enum(Ord0, pred, succ) {
    this.Ord0 = Ord0;
    this.pred = pred;
    this.succ = succ;
  };

  var BoundedEnum = function BoundedEnum(Bounded0, Enum1, cardinality, fromEnum, toEnum) {
    this.Bounded0 = Bounded0;
    this.Enum1 = Enum1;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.toEnum = toEnum;
  };

  var toEnum = function toEnum(dict) {
    return dict.toEnum;
  };

  var fromEnum = function fromEnum(dict) {
    return dict.fromEnum;
  };

  var toEnumWithDefaults = function toEnumWithDefaults(dictBoundedEnum) {
    return function (low) {
      return function (high) {
        return function (x) {
          var v = toEnum(dictBoundedEnum)(x);

          if (v instanceof Data_Maybe.Just) {
            return v.value0;
          }

          ;

          if (v instanceof Data_Maybe.Nothing) {
            var $54 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));

            if ($54) {
              return low;
            }

            ;
            return high;
          }

          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };

  var defaultSucc = function defaultSucc(toEnum$prime) {
    return function (fromEnum$prime) {
      return function (a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };

  var defaultPred = function defaultPred(toEnum$prime) {
    return function (fromEnum$prime) {
      return function (a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };

  var charToEnum = function charToEnum(v) {
    if (v >= Data_Bounded.bottom(Data_Bounded.boundedInt) && v <= Data_Bounded.top(Data_Bounded.boundedInt)) {
      return new Data_Maybe.Just($foreign.fromCharCode(v));
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var enumChar = new Enum(function () {
    return Data_Ord.ordChar;
  }, defaultPred(charToEnum)($foreign.toCharCode), defaultSucc(charToEnum)($foreign.toCharCode));
  var boundedEnumChar = new BoundedEnum(function () {
    return Data_Bounded.boundedChar;
  }, function () {
    return enumChar;
  }, $foreign.toCharCode(Data_Bounded.top(Data_Bounded.boundedChar)) - $foreign.toCharCode(Data_Bounded.bottom(Data_Bounded.boundedChar)) | 0, $foreign.toCharCode, charToEnum);
  exports["Enum"] = Enum;
  exports["BoundedEnum"] = BoundedEnum;
  exports["toEnum"] = toEnum;
  exports["fromEnum"] = fromEnum;
  exports["toEnumWithDefaults"] = toEnumWithDefaults;
  exports["boundedEnumChar"] = boundedEnumChar;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Date.Component"] = $PS["Data.Date.Component"] || {};
  var exports = $PS["Data.Date.Component"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Show = $PS["Data.Show"];

  var Monday = function () {
    function Monday() {}

    ;
    Monday.value = new Monday();
    return Monday;
  }();

  var Tuesday = function () {
    function Tuesday() {}

    ;
    Tuesday.value = new Tuesday();
    return Tuesday;
  }();

  var Wednesday = function () {
    function Wednesday() {}

    ;
    Wednesday.value = new Wednesday();
    return Wednesday;
  }();

  var Thursday = function () {
    function Thursday() {}

    ;
    Thursday.value = new Thursday();
    return Thursday;
  }();

  var Friday = function () {
    function Friday() {}

    ;
    Friday.value = new Friday();
    return Friday;
  }();

  var Saturday = function () {
    function Saturday() {}

    ;
    Saturday.value = new Saturday();
    return Saturday;
  }();

  var Sunday = function () {
    function Sunday() {}

    ;
    Sunday.value = new Sunday();
    return Sunday;
  }();

  var January = function () {
    function January() {}

    ;
    January.value = new January();
    return January;
  }();

  var February = function () {
    function February() {}

    ;
    February.value = new February();
    return February;
  }();

  var March = function () {
    function March() {}

    ;
    March.value = new March();
    return March;
  }();

  var April = function () {
    function April() {}

    ;
    April.value = new April();
    return April;
  }();

  var May = function () {
    function May() {}

    ;
    May.value = new May();
    return May;
  }();

  var June = function () {
    function June() {}

    ;
    June.value = new June();
    return June;
  }();

  var July = function () {
    function July() {}

    ;
    July.value = new July();
    return July;
  }();

  var August = function () {
    function August() {}

    ;
    August.value = new August();
    return August;
  }();

  var September = function () {
    function September() {}

    ;
    September.value = new September();
    return September;
  }();

  var October = function () {
    function October() {}

    ;
    October.value = new October();
    return October;
  }();

  var November = function () {
    function November() {}

    ;
    November.value = new November();
    return November;
  }();

  var December = function () {
    function December() {}

    ;
    December.value = new December();
    return December;
  }();

  var showWeekday = new Data_Show.Show(function (v) {
    if (v instanceof Monday) {
      return "Monday";
    }

    ;

    if (v instanceof Tuesday) {
      return "Tuesday";
    }

    ;

    if (v instanceof Wednesday) {
      return "Wednesday";
    }

    ;

    if (v instanceof Thursday) {
      return "Thursday";
    }

    ;

    if (v instanceof Friday) {
      return "Friday";
    }

    ;

    if (v instanceof Saturday) {
      return "Saturday";
    }

    ;

    if (v instanceof Sunday) {
      return "Sunday";
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 184, column 1 - line 191, column 25): " + [v.constructor.name]);
  });
  var showMonth = new Data_Show.Show(function (v) {
    if (v instanceof January) {
      return "January";
    }

    ;

    if (v instanceof February) {
      return "February";
    }

    ;

    if (v instanceof March) {
      return "March";
    }

    ;

    if (v instanceof April) {
      return "April";
    }

    ;

    if (v instanceof May) {
      return "May";
    }

    ;

    if (v instanceof June) {
      return "June";
    }

    ;

    if (v instanceof July) {
      return "July";
    }

    ;

    if (v instanceof August) {
      return "August";
    }

    ;

    if (v instanceof September) {
      return "September";
    }

    ;

    if (v instanceof October) {
      return "October";
    }

    ;

    if (v instanceof November) {
      return "November";
    }

    ;

    if (v instanceof December) {
      return "December";
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 101, column 1 - line 113, column 29): " + [v.constructor.name]);
  });
  var ordYear = Data_Ord.ordInt;
  var ordDay = Data_Ord.ordInt;
  var eqWeekday = new Data_Eq.Eq(function (x) {
    return function (y) {
      if (x instanceof Monday && y instanceof Monday) {
        return true;
      }

      ;

      if (x instanceof Tuesday && y instanceof Tuesday) {
        return true;
      }

      ;

      if (x instanceof Wednesday && y instanceof Wednesday) {
        return true;
      }

      ;

      if (x instanceof Thursday && y instanceof Thursday) {
        return true;
      }

      ;

      if (x instanceof Friday && y instanceof Friday) {
        return true;
      }

      ;

      if (x instanceof Saturday && y instanceof Saturday) {
        return true;
      }

      ;

      if (x instanceof Sunday && y instanceof Sunday) {
        return true;
      }

      ;
      return false;
    };
  });
  var ordWeekday = new Data_Ord.Ord(function () {
    return eqWeekday;
  }, function (x) {
    return function (y) {
      if (x instanceof Monday && y instanceof Monday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Monday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Monday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Tuesday && y instanceof Tuesday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Tuesday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Tuesday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Wednesday && y instanceof Wednesday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Wednesday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Wednesday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Thursday && y instanceof Thursday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Thursday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Thursday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Friday && y instanceof Friday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Friday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Friday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Saturday && y instanceof Saturday) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof Saturday) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof Saturday) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof Sunday && y instanceof Sunday) {
        return Data_Ordering.EQ.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 154, column 1 - line 154, column 42): " + [x.constructor.name, y.constructor.name]);
    };
  });
  var eqMonth = new Data_Eq.Eq(function (x) {
    return function (y) {
      if (x instanceof January && y instanceof January) {
        return true;
      }

      ;

      if (x instanceof February && y instanceof February) {
        return true;
      }

      ;

      if (x instanceof March && y instanceof March) {
        return true;
      }

      ;

      if (x instanceof April && y instanceof April) {
        return true;
      }

      ;

      if (x instanceof May && y instanceof May) {
        return true;
      }

      ;

      if (x instanceof June && y instanceof June) {
        return true;
      }

      ;

      if (x instanceof July && y instanceof July) {
        return true;
      }

      ;

      if (x instanceof August && y instanceof August) {
        return true;
      }

      ;

      if (x instanceof September && y instanceof September) {
        return true;
      }

      ;

      if (x instanceof October && y instanceof October) {
        return true;
      }

      ;

      if (x instanceof November && y instanceof November) {
        return true;
      }

      ;

      if (x instanceof December && y instanceof December) {
        return true;
      }

      ;
      return false;
    };
  });
  var ordMonth = new Data_Ord.Ord(function () {
    return eqMonth;
  }, function (x) {
    return function (y) {
      if (x instanceof January && y instanceof January) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof January) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof January) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof February && y instanceof February) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof February) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof February) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof March && y instanceof March) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof March) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof March) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof April && y instanceof April) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof April) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof April) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof May && y instanceof May) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof May) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof May) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof June && y instanceof June) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof June) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof June) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof July && y instanceof July) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof July) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof July) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof August && y instanceof August) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof August) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof August) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof September && y instanceof September) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof September) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof September) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof October && y instanceof October) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof October) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof October) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof November && y instanceof November) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof November) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof November) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof December && y instanceof December) {
        return Data_Ordering.EQ.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 61, column 1 - line 61, column 38): " + [x.constructor.name, y.constructor.name]);
    };
  });
  var boundedYear = new Data_Bounded.Bounded(function () {
    return ordYear;
  }, -271820 | 0, 275759);
  var boundedWeekday = new Data_Bounded.Bounded(function () {
    return ordWeekday;
  }, Monday.value, Sunday.value);
  var boundedMonth = new Data_Bounded.Bounded(function () {
    return ordMonth;
  }, January.value, December.value);
  var boundedEnumYear = new Data_Enum.BoundedEnum(function () {
    return boundedYear;
  }, function () {
    return enumYear;
  }, 547580, function (v) {
    return v;
  }, function (n) {
    if (n >= (-271820 | 0) && n <= 275759) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
  });
  var enumYear = new Data_Enum.Enum(function () {
    return ordYear;
  }, function () {
    var $46 = Data_Enum.toEnum(boundedEnumYear);
    var $47 = Data_Enum.fromEnum(boundedEnumYear);
    return function ($48) {
      return $46(function (v) {
        return v - 1 | 0;
      }($47($48)));
    };
  }(), function () {
    var $49 = Data_Enum.toEnum(boundedEnumYear);
    var $50 = Data_Enum.fromEnum(boundedEnumYear);
    return function ($51) {
      return $49(function (v) {
        return v + 1 | 0;
      }($50($51)));
    };
  }());
  var boundedEnumWeekday = new Data_Enum.BoundedEnum(function () {
    return boundedWeekday;
  }, function () {
    return enumWeekday;
  }, 7, function (v) {
    if (v instanceof Monday) {
      return 1;
    }

    ;

    if (v instanceof Tuesday) {
      return 2;
    }

    ;

    if (v instanceof Wednesday) {
      return 3;
    }

    ;

    if (v instanceof Thursday) {
      return 4;
    }

    ;

    if (v instanceof Friday) {
      return 5;
    }

    ;

    if (v instanceof Saturday) {
      return 6;
    }

    ;

    if (v instanceof Sunday) {
      return 7;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 175, column 14 - line 182, column 16): " + [v.constructor.name]);
  }, function (v) {
    if (v === 1) {
      return new Data_Maybe.Just(Monday.value);
    }

    ;

    if (v === 2) {
      return new Data_Maybe.Just(Tuesday.value);
    }

    ;

    if (v === 3) {
      return new Data_Maybe.Just(Wednesday.value);
    }

    ;

    if (v === 4) {
      return new Data_Maybe.Just(Thursday.value);
    }

    ;

    if (v === 5) {
      return new Data_Maybe.Just(Friday.value);
    }

    ;

    if (v === 6) {
      return new Data_Maybe.Just(Saturday.value);
    }

    ;

    if (v === 7) {
      return new Data_Maybe.Just(Sunday.value);
    }

    ;
    return Data_Maybe.Nothing.value;
  });
  var enumWeekday = new Data_Enum.Enum(function () {
    return ordWeekday;
  }, function () {
    var $52 = Data_Enum.toEnum(boundedEnumWeekday);
    var $53 = Data_Enum.fromEnum(boundedEnumWeekday);
    return function ($54) {
      return $52(function (v) {
        return v - 1 | 0;
      }($53($54)));
    };
  }(), function () {
    var $55 = Data_Enum.toEnum(boundedEnumWeekday);
    var $56 = Data_Enum.fromEnum(boundedEnumWeekday);
    return function ($57) {
      return $55(function (v) {
        return v + 1 | 0;
      }($56($57)));
    };
  }());
  var boundedEnumMonth = new Data_Enum.BoundedEnum(function () {
    return boundedMonth;
  }, function () {
    return enumMonth;
  }, 12, function (v) {
    if (v instanceof January) {
      return 1;
    }

    ;

    if (v instanceof February) {
      return 2;
    }

    ;

    if (v instanceof March) {
      return 3;
    }

    ;

    if (v instanceof April) {
      return 4;
    }

    ;

    if (v instanceof May) {
      return 5;
    }

    ;

    if (v instanceof June) {
      return 6;
    }

    ;

    if (v instanceof July) {
      return 7;
    }

    ;

    if (v instanceof August) {
      return 8;
    }

    ;

    if (v instanceof September) {
      return 9;
    }

    ;

    if (v instanceof October) {
      return 10;
    }

    ;

    if (v instanceof November) {
      return 11;
    }

    ;

    if (v instanceof December) {
      return 12;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
  }, function (v) {
    if (v === 1) {
      return new Data_Maybe.Just(January.value);
    }

    ;

    if (v === 2) {
      return new Data_Maybe.Just(February.value);
    }

    ;

    if (v === 3) {
      return new Data_Maybe.Just(March.value);
    }

    ;

    if (v === 4) {
      return new Data_Maybe.Just(April.value);
    }

    ;

    if (v === 5) {
      return new Data_Maybe.Just(May.value);
    }

    ;

    if (v === 6) {
      return new Data_Maybe.Just(June.value);
    }

    ;

    if (v === 7) {
      return new Data_Maybe.Just(July.value);
    }

    ;

    if (v === 8) {
      return new Data_Maybe.Just(August.value);
    }

    ;

    if (v === 9) {
      return new Data_Maybe.Just(September.value);
    }

    ;

    if (v === 10) {
      return new Data_Maybe.Just(October.value);
    }

    ;

    if (v === 11) {
      return new Data_Maybe.Just(November.value);
    }

    ;

    if (v === 12) {
      return new Data_Maybe.Just(December.value);
    }

    ;
    return Data_Maybe.Nothing.value;
  });
  var enumMonth = new Data_Enum.Enum(function () {
    return ordMonth;
  }, function () {
    var $58 = Data_Enum.toEnum(boundedEnumMonth);
    var $59 = Data_Enum.fromEnum(boundedEnumMonth);
    return function ($60) {
      return $58(function (v) {
        return v - 1 | 0;
      }($59($60)));
    };
  }(), function () {
    var $61 = Data_Enum.toEnum(boundedEnumMonth);
    var $62 = Data_Enum.fromEnum(boundedEnumMonth);
    return function ($63) {
      return $61(function (v) {
        return v + 1 | 0;
      }($62($63)));
    };
  }());
  var boundedDay = new Data_Bounded.Bounded(function () {
    return ordDay;
  }, 1, 31);
  var boundedEnumDay = new Data_Enum.BoundedEnum(function () {
    return boundedDay;
  }, function () {
    return enumDay;
  }, 31, function (v) {
    return v;
  }, function (n) {
    if (n >= 1 && n <= 31) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
  });
  var enumDay = new Data_Enum.Enum(function () {
    return ordDay;
  }, function () {
    var $64 = Data_Enum.toEnum(boundedEnumDay);
    var $65 = Data_Enum.fromEnum(boundedEnumDay);
    return function ($66) {
      return $64(function (v) {
        return v - 1 | 0;
      }($65($66)));
    };
  }(), function () {
    var $67 = Data_Enum.toEnum(boundedEnumDay);
    var $68 = Data_Enum.fromEnum(boundedEnumDay);
    return function ($69) {
      return $67(function (v) {
        return v + 1 | 0;
      }($68($69)));
    };
  }());
  exports["January"] = January;
  exports["February"] = February;
  exports["March"] = March;
  exports["April"] = April;
  exports["May"] = May;
  exports["June"] = June;
  exports["July"] = July;
  exports["August"] = August;
  exports["September"] = September;
  exports["October"] = October;
  exports["November"] = November;
  exports["December"] = December;
  exports["boundedEnumYear"] = boundedEnumYear;
  exports["boundedEnumMonth"] = boundedEnumMonth;
  exports["showMonth"] = showMonth;
  exports["boundedEnumDay"] = boundedEnumDay;
  exports["boundedEnumWeekday"] = boundedEnumWeekday;
  exports["showWeekday"] = showWeekday;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Date"] = $PS["Data.Date"] || {};
  var exports = $PS["Data.Date"];
  var $foreign = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Maybe = $PS["Data.Maybe"];

  var $$Date = function () {
    function $$Date(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    $$Date.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new $$Date(value0, value1, value2);
        };
      };
    };

    return $$Date;
  }();

  var year = function year(v) {
    return v.value0;
  };

  var weekday = function weekday(v) {
    var n = $foreign.calcWeekday(v.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v.value1), v.value2);
    var $41 = n === 0;

    if ($41) {
      return Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumWeekday)(7));
    }

    ;
    return Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumWeekday)(n));
  };

  var month = function month(v) {
    return v.value1;
  };

  var day = function day(v) {
    return v.value2;
  };

  var canonicalDate = function canonicalDate(y) {
    return function (m) {
      return function (d) {
        var mkDate = function mkDate(y$prime) {
          return function (m$prime) {
            return function (d$prime) {
              return new $$Date(y$prime, Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(m$prime)), d$prime);
            };
          };
        };

        return $foreign.canonicalDateImpl(mkDate, y, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(m), d);
      };
    };
  };

  exports["canonicalDate"] = canonicalDate;
  exports["year"] = year;
  exports["month"] = month;
  exports["day"] = day;
  exports["weekday"] = weekday;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.DateTime"] = $PS["Data.DateTime"] || {};
  var exports = $PS["Data.DateTime"];

  var DateTime = function () {
    function DateTime(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    DateTime.create = function (value0) {
      return function (value1) {
        return new DateTime(value0, value1);
      };
    };

    return DateTime;
  }();

  var time = function time(v) {
    return v.value1;
  };

  var date = function date(v) {
    return v.value0;
  };

  exports["DateTime"] = DateTime;
  exports["date"] = date;
  exports["time"] = time;
})(PS);

(function (exports) {
  "use strict";

  var createDateTime = function createDateTime(y, m, d, h, mi, s, ms) {
    var dateTime = new Date(Date.UTC(y, m, d, h, mi, s, ms));

    if (y >= 0 && y < 100) {
      dateTime.setUTCFullYear(y);
    }

    return dateTime;
  };

  exports.fromDateTimeImpl = function (y, mo, d, h, mi, s, ms) {
    return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
  };

  exports.toDateTimeImpl = function (ctor) {
    return function (instant) {
      var dt = new Date(instant);
      return ctor(dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
    };
  };
})(PS["Data.DateTime.Instant"] = PS["Data.DateTime.Instant"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Time"] = $PS["Data.Time"] || {};
  var exports = $PS["Data.Time"];

  var Time = function () {
    function Time(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Time.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Time(value0, value1, value2, value3);
          };
        };
      };
    };

    return Time;
  }();

  var second = function second(v) {
    return v.value2;
  };

  var minute = function minute(v) {
    return v.value1;
  };

  var millisecond = function millisecond(v) {
    return v.value3;
  };

  var hour = function hour(v) {
    return v.value0;
  };

  exports["Time"] = Time;
  exports["hour"] = hour;
  exports["minute"] = minute;
  exports["second"] = second;
  exports["millisecond"] = millisecond;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.DateTime.Instant"] = $PS["Data.DateTime.Instant"] || {};
  var exports = $PS["Data.DateTime.Instant"];
  var $foreign = $PS["Data.DateTime.Instant"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_DateTime = $PS["Data.DateTime"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Time = $PS["Data.Time"];

  var unInstant = function unInstant(v) {
    return v;
  };

  var toDateTime = function () {
    var mkDateTime = function mkDateTime(y) {
      return function (mo) {
        return function (d) {
          return function (h) {
            return function (mi) {
              return function (s) {
                return function (ms) {
                  return new Data_DateTime.DateTime(Data_Date.canonicalDate(y)(Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(mo)))(d), new Data_Time.Time(h, mi, s, ms));
                };
              };
            };
          };
        };
      };
    };

    return $foreign.toDateTimeImpl(mkDateTime);
  }();

  var fromDateTime = function fromDateTime(v) {
    return $foreign.fromDateTimeImpl(Data_Date.year(v.value0), Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)), Data_Date.day(v.value0), Data_Time.hour(v.value1), Data_Time.minute(v.value1), Data_Time.second(v.value1), Data_Time.millisecond(v.value1));
  };

  exports["unInstant"] = unInstant;
  exports["fromDateTime"] = fromDateTime;
  exports["toDateTime"] = toDateTime;
})(PS);

(function (exports) {
  "use strict";

  exports.intDegree = function (x) {
    return Math.min(Math.abs(x), 2147483647);
  }; // See the Euclidean definition in
  // https://en.m.wikipedia.org/wiki/Modulo_operation.


  exports.intDiv = function (x) {
    return function (y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };

  exports.intMod = function (x) {
    return function (y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };
})(PS["Data.EuclideanRing"] = PS["Data.EuclideanRing"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.EuclideanRing"] = $PS["Data.EuclideanRing"] || {};
  var exports = $PS["Data.EuclideanRing"];
  var $foreign = $PS["Data.EuclideanRing"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];

  var EuclideanRing = function EuclideanRing(CommutativeRing0, degree, div, mod) {
    this.CommutativeRing0 = CommutativeRing0;
    this.degree = degree;
    this.div = div;
    this.mod = mod;
  };

  var mod = function mod(dict) {
    return dict.mod;
  };

  var euclideanRingInt = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingInt;
  }, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);

  var div = function div(dict) {
    return dict.div;
  };

  exports["div"] = div;
  exports["mod"] = mod;
  exports["euclideanRingInt"] = euclideanRingInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.String.Pattern"] = $PS["Data.String.Pattern"] || {};
  var exports = $PS["Data.String.Pattern"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Pattern = function Pattern(x) {
    return x;
  };

  var newtypePattern = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Pattern);
  exports["newtypePattern"] = newtypePattern;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Text.Parsing.Parser.Pos"] = $PS["Text.Parsing.Parser.Pos"] || {};
  var exports = $PS["Text.Parsing.Parser.Pos"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_String_Pattern = $PS["Data.String.Pattern"];

  var updatePosString = function updatePosString(pos$prime) {
    return function (str) {
      var updatePosChar = function updatePosChar(v) {
        return function (c) {
          if (c === "\x0a") {
            return {
              line: v.line + 1 | 0,
              column: 1
            };
          }

          ;

          if (c === "\x0d") {
            return {
              line: v.line + 1 | 0,
              column: 1
            };
          }

          ;

          if (c === "\x09") {
            return {
              line: v.line,
              column: (v.column + 8 | 0) - Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v.column - 1 | 0)(8) | 0
            };
          }

          ;
          return {
            line: v.line,
            column: v.column + 1 | 0
          };
        };
      };

      return Data_Foldable.foldl(Data_Foldable.foldableArray)(updatePosChar)(pos$prime)(Data_String_Common.split(Data_Newtype.wrap(Data_String_Pattern.newtypePattern)(""))(str));
    };
  };

  var initialPos = {
    line: 1,
    column: 1
  };
  exports["initialPos"] = initialPos;
  exports["updatePosString"] = updatePosString;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Text.Parsing.Parser"] = $PS["Text.Parsing.Parser"] || {};
  var exports = $PS["Text.Parsing.Parser"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Alternative = $PS["Control.Alternative"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Lazy = $PS["Control.Lazy"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_State_Trans = $PS["Control.Monad.State.Trans"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Either = $PS["Data.Either"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Text_Parsing_Parser_Pos = $PS["Text.Parsing.Parser.Pos"];

  var ParseState = function () {
    function ParseState(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    ParseState.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new ParseState(value0, value1, value2);
        };
      };
    };

    return ParseState;
  }();

  var ParseError = function () {
    function ParseError(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ParseError.create = function (value0) {
      return function (value1) {
        return new ParseError(value0, value1);
      };
    };

    return ParseError;
  }();

  var ParserT = function ParserT(x) {
    return x;
  };

  var parseErrorPosition = function parseErrorPosition(v) {
    return v.value1;
  };

  var parseErrorMessage = function parseErrorMessage(v) {
    return v.value0;
  };

  var newtypeParserT = new Data_Newtype.Newtype(function (n) {
    return n;
  }, ParserT);

  var runParserT = function runParserT(dictMonad) {
    return function (s) {
      return function (p) {
        var initialState = new ParseState(s, Text_Parsing_Parser_Pos.initialPos, false);
        return Control_Monad_State_Trans.evalStateT(dictMonad.Bind1().Apply0().Functor0())(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p)))(initialState);
      };
    };
  };

  var runParser = function runParser(s) {
    var $90 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
    var $91 = runParserT(Data_Identity.monadIdentity)(s);
    return function ($92) {
      return $90($91($92));
    };
  };

  var monadThrowParserT = function monadThrowParserT(dictMonad) {
    return Control_Monad_Except_Trans.monadThrowExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };

  var monadStateParserT = function monadStateParserT(dictMonad) {
    return Control_Monad_Except_Trans.monadStateExceptT(Control_Monad_State_Trans.monadStateStateT(dictMonad));
  };

  var position = function position(dictMonad) {
    return Control_Monad_State_Class.gets(monadStateParserT(dictMonad))(function (v) {
      return v.value1;
    });
  };

  var lazyParserT = new Control_Lazy.Lazy(function (f) {
    return Control_Lazy.defer(Control_Monad_State_Trans.lazyStateT)(function () {
      var $98 = Data_Newtype.unwrap(newtypeParserT);
      return function ($99) {
        return Control_Monad_Except_Trans.runExceptT($98(f($99)));
      };
    }());
  });

  var functorParserT = function functorParserT(dictFunctor) {
    return Control_Monad_Except_Trans.functorExceptT(Control_Monad_State_Trans.functorStateT(dictFunctor));
  };

  var failWithPosition = function failWithPosition(dictMonad) {
    return function (message) {
      return function (pos) {
        return Control_Monad_Error_Class.throwError(monadThrowParserT(dictMonad))(new ParseError(message, pos));
      };
    };
  };

  var bindParserT = function bindParserT(dictMonad) {
    return Control_Monad_Except_Trans.bindExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };

  var fail = function fail(dictMonad) {
    return function (message) {
      return Control_Bind.bindFlipped(bindParserT(dictMonad))(failWithPosition(dictMonad)(message))(position(dictMonad));
    };
  };

  var applyParserT = function applyParserT(dictMonad) {
    return Control_Monad_Except_Trans.applyExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };

  var applicativeParserT = function applicativeParserT(dictMonad) {
    return Control_Monad_Except_Trans.applicativeExceptT(Control_Monad_State_Trans.monadStateT(dictMonad));
  };

  var altParserT = function altParserT(dictMonad) {
    return new Control_Alt.Alt(function () {
      return functorParserT(dictMonad.Bind1().Apply0().Functor0());
    }, function (p1) {
      return function (p2) {
        return ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
          return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p1)))(new ParseState(v.value0, v.value1, false)))(function (v1) {
            if (v1.value0 instanceof Data_Either.Left && !v1.value1.value2) {
              return Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(newtypeParserT)(p2)))(v);
            }

            ;
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
          });
        })));
      };
    });
  };

  var plusParserT = function plusParserT(dictMonad) {
    return new Control_Plus.Plus(function () {
      return altParserT(dictMonad);
    }, fail(dictMonad)("No alternative"));
  };

  var alternativeParserT = function alternativeParserT(dictMonad) {
    return new Control_Alternative.Alternative(function () {
      return applicativeParserT(dictMonad);
    }, function () {
      return plusParserT(dictMonad);
    });
  };

  exports["ParseError"] = ParseError;
  exports["parseErrorMessage"] = parseErrorMessage;
  exports["parseErrorPosition"] = parseErrorPosition;
  exports["ParseState"] = ParseState;
  exports["ParserT"] = ParserT;
  exports["runParser"] = runParser;
  exports["fail"] = fail;
  exports["newtypeParserT"] = newtypeParserT;
  exports["lazyParserT"] = lazyParserT;
  exports["functorParserT"] = functorParserT;
  exports["applyParserT"] = applyParserT;
  exports["applicativeParserT"] = applicativeParserT;
  exports["bindParserT"] = bindParserT;
  exports["monadStateParserT"] = monadStateParserT;
  exports["altParserT"] = altParserT;
  exports["plusParserT"] = plusParserT;
  exports["alternativeParserT"] = alternativeParserT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Text.Parsing.Parser.Combinators"] = $PS["Text.Parsing.Parser.Combinators"] || {};
  var exports = $PS["Text.Parsing.Parser.Combinators"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_State_Trans = $PS["Control.Monad.State.Trans"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];

  var withErrorMessage = function withErrorMessage(dictMonad) {
    return function (p) {
      return function (msg) {
        return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(p)(Text_Parsing_Parser.fail(dictMonad)("Expected " + msg));
      };
    };
  };

  var tryRethrow = function tryRethrow(dictMonad) {
    return function (p) {
      return Text_Parsing_Parser.ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
        return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(Text_Parsing_Parser.newtypeParserT)(p)))(v))(function (v1) {
          if (v1.value0 instanceof Data_Either.Left) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(new Data_Either.Left(new Text_Parsing_Parser.ParseError(v1.value0.value0.value0, v.value1)), new Text_Parsing_Parser.ParseState(v1.value1.value0, v1.value1.value1, v.value2)));
          }

          ;
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
        });
      })));
    };
  };

  var $$try = function $$try(dictMonad) {
    return function (p) {
      return Text_Parsing_Parser.ParserT(Control_Monad_Except_Trans.ExceptT(Control_Monad_State_Trans.StateT(function (v) {
        return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_State_Trans.runStateT(Control_Monad_Except_Trans.runExceptT(Data_Newtype.unwrap(Text_Parsing_Parser.newtypeParserT)(p)))(v))(function (v1) {
          if (v1.value0 instanceof Data_Either.Left) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, new Text_Parsing_Parser.ParseState(v1.value1.value0, v1.value1.value1, v.value2)));
          }

          ;
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value1));
        });
      })));
    };
  };

  var choice = function choice(dictFoldable) {
    return function (dictMonad) {
      return Data_Foldable.foldl(dictFoldable)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad)))(Control_Plus.empty(Text_Parsing_Parser.plusParserT(dictMonad)));
    };
  };

  exports["withErrorMessage"] = withErrorMessage;
  exports["try"] = $$try;
  exports["tryRethrow"] = tryRethrow;
  exports["choice"] = choice;
})(PS);

(function (exports) {
  "use strict";
  /* global Symbol */

  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";

  exports._unsafeCodePointAt0 = function (fallback) {
    return hasCodePointAt ? function (str) {
      return str.codePointAt(0);
    } : fallback;
  };

  exports._singleton = function (fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };

  exports._take = function (fallback) {
    return function (n) {
      if (hasStringIterator) {
        return function (str) {
          var accum = "";
          var iter = str[Symbol.iterator]();

          for (var i = 0; i < n; ++i) {
            var o = iter.next();
            if (o.done) return accum;
            accum += o.value;
          }

          return accum;
        };
      }

      return fallback(n);
    };
  };

  exports._toCodePointArray = function (fallback) {
    return function (unsafeCodePointAt0) {
      if (hasArrayFrom) {
        return function (str) {
          return Array.from(str, unsafeCodePointAt0);
        };
      }

      return fallback;
    };
  };
})(PS["Data.String.CodePoints"] = PS["Data.String.CodePoints"] || {});

(function (exports) {
  "use strict";

  exports.fromCharArray = function (a) {
    return a.join("");
  };

  exports.toCharArray = function (s) {
    return s.split("");
  };

  exports.singleton = function (c) {
    return c;
  };

  exports._charAt = function (just) {
    return function (nothing) {
      return function (i) {
        return function (s) {
          return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
        };
      };
    };
  };

  exports.length = function (s) {
    return s.length;
  };

  exports._indexOf = function (just) {
    return function (nothing) {
      return function (x) {
        return function (s) {
          var i = s.indexOf(x);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };

  exports.take = function (n) {
    return function (s) {
      return s.substr(0, n);
    };
  };

  exports.drop = function (n) {
    return function (s) {
      return s.substring(n);
    };
  };
})(PS["Data.String.CodeUnits"] = PS["Data.String.CodeUnits"] || {});

(function (exports) {
  "use strict";

  exports.charAt = function (i) {
    return function (s) {
      if (i >= 0 && i < s.length) return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };
})(PS["Data.String.Unsafe"] = PS["Data.String.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.String.Unsafe"] = $PS["Data.String.Unsafe"] || {};
  var exports = $PS["Data.String.Unsafe"];
  var $foreign = $PS["Data.String.Unsafe"];
  exports["charAt"] = $foreign.charAt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.String.CodeUnits"] = $PS["Data.String.CodeUnits"] || {};
  var exports = $PS["Data.String.CodeUnits"];
  var $foreign = $PS["Data.String.CodeUnits"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_Unsafe = $PS["Data.String.Unsafe"];

  var uncons = function uncons(v) {
    if (v === "") {
      return Data_Maybe.Nothing.value;
    }

    ;
    return new Data_Maybe.Just({
      head: Data_String_Unsafe.charAt(0)(v),
      tail: $foreign.drop(1)(v)
    });
  };

  var indexOf = $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var stripPrefix = function stripPrefix(v) {
    return function (str) {
      var v1 = indexOf(v)(str);

      if (v1 instanceof Data_Maybe.Just && v1.value0 === 0) {
        return Data_Maybe.Just.create($foreign.drop($foreign.length(v))(str));
      }

      ;
      return Data_Maybe.Nothing.value;
    };
  };

  var contains = function contains(pat) {
    var $16 = indexOf(pat);
    return function ($17) {
      return Data_Maybe.isJust($16($17));
    };
  };

  var charAt = $foreign["_charAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["stripPrefix"] = stripPrefix;
  exports["contains"] = contains;
  exports["uncons"] = uncons;
  exports["indexOf"] = indexOf;
  exports["singleton"] = $foreign.singleton;
  exports["fromCharArray"] = $foreign.fromCharArray;
  exports["toCharArray"] = $foreign.toCharArray;
  exports["length"] = $foreign.length;
  exports["take"] = $foreign.take;
  exports["drop"] = $foreign.drop;
})(PS);

(function (exports) {
  "use strict";

  exports.unfoldrArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;

              while (true) {
                // eslint-disable-line no-constant-condition
                var maybe = f(value);
                if (isNothing(maybe)) return result;
                var tuple = fromJust(maybe);
                result.push(fst(tuple));
                value = snd(tuple);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable"] = PS["Data.Unfoldable"] || {});

(function (exports) {
  "use strict";

  exports.unfoldr1ArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;

              while (true) {
                // eslint-disable-line no-constant-condition
                var tuple = f(value);
                result.push(fst(tuple));
                var maybe = snd(tuple);
                if (isNothing(maybe)) return result;
                value = fromJust(maybe);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable1"] = PS["Data.Unfoldable1"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Unfoldable1"] = $PS["Data.Unfoldable1"] || {};
  var exports = $PS["Data.Unfoldable1"];
  var $foreign = $PS["Data.Unfoldable1"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Unfoldable1 = function Unfoldable1(unfoldr1) {
    this.unfoldr1 = unfoldr1;
  };

  var unfoldable1Array = new Unfoldable1($foreign.unfoldr1ArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["unfoldable1Array"] = unfoldable1Array;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Unfoldable"] = $PS["Data.Unfoldable"] || {};
  var exports = $PS["Data.Unfoldable"];
  var $foreign = $PS["Data.Unfoldable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable1 = $PS["Data.Unfoldable1"];

  var Unfoldable = function Unfoldable(Unfoldable10, unfoldr) {
    this.Unfoldable10 = Unfoldable10;
    this.unfoldr = unfoldr;
  };

  var unfoldr = function unfoldr(dict) {
    return dict.unfoldr;
  };

  var unfoldableArray = new Unfoldable(function () {
    return Data_Unfoldable1.unfoldable1Array;
  }, $foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["unfoldr"] = unfoldr;
  exports["unfoldableArray"] = unfoldableArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.String.CodePoints"] = $PS["Data.String.CodePoints"] || {};
  var exports = $PS["Data.String.CodePoints"];
  var $foreign = $PS["Data.String.CodePoints"];
  var Data_Array = $PS["Data.Array"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Unsafe = $PS["Data.String.Unsafe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];

  var unsurrogate = function unsurrogate(lead) {
    return function (trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };

  var isTrail = function isTrail(cu) {
    return 56320 <= cu && cu <= 57343;
  };

  var isLead = function isLead(cu) {
    return 55296 <= cu && cu <= 56319;
  };

  var uncons = function uncons(s) {
    var v = Data_String_CodeUnits.length(s);

    if (v === 0) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v === 1) {
      return new Data_Maybe.Just({
        head: Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s)),
        tail: ""
      });
    }

    ;
    var cu1 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(1)(s));
    var cu0 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s));
    var $21 = isLead(cu0) && isTrail(cu1);

    if ($21) {
      return new Data_Maybe.Just({
        head: unsurrogate(cu0)(cu1),
        tail: Data_String_CodeUnits.drop(2)(s)
      });
    }

    ;
    return new Data_Maybe.Just({
      head: cu0,
      tail: Data_String_CodeUnits.drop(1)(s)
    });
  };

  var unconsButWithTuple = function unconsButWithTuple(s) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
      return new Data_Tuple.Tuple(v.head, v.tail);
    })(uncons(s));
  };

  var toCodePointArrayFallback = function toCodePointArrayFallback(s) {
    return Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray)(unconsButWithTuple)(s);
  };

  var unsafeCodePointAt0Fallback = function unsafeCodePointAt0Fallback(s) {
    var cu0 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(0)(s));
    var $25 = isLead(cu0) && Data_String_CodeUnits.length(s) > 1;

    if ($25) {
      var cu1 = Data_Enum.fromEnum(Data_Enum.boundedEnumChar)(Data_String_Unsafe.charAt(1)(s));
      var $26 = isTrail(cu1);

      if ($26) {
        return unsurrogate(cu0)(cu1);
      }

      ;
      return cu0;
    }

    ;
    return cu0;
  };

  var unsafeCodePointAt0 = $foreign["_unsafeCodePointAt0"](unsafeCodePointAt0Fallback);
  var toCodePointArray = $foreign["_toCodePointArray"](toCodePointArrayFallback)(unsafeCodePointAt0);

  var length = function length($52) {
    return Data_Array.length(toCodePointArray($52));
  };

  var indexOf = function indexOf(p) {
    return function (s) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(function (i) {
        return length(Data_String_CodeUnits.take(i)(s));
      })(Data_String_CodeUnits.indexOf(p)(s));
    };
  };

  var fromCharCode = function () {
    var $53 = Data_Enum.toEnumWithDefaults(Data_Enum.boundedEnumChar)(Data_Bounded.bottom(Data_Bounded.boundedChar))(Data_Bounded.top(Data_Bounded.boundedChar));
    return function ($54) {
      return Data_String_CodeUnits.singleton($53($54));
    };
  }();

  var singletonFallback = function singletonFallback(v) {
    if (v <= 65535) {
      return fromCharCode(v);
    }

    ;
    var lead = Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode(lead) + fromCharCode(trail);
  };

  var singleton = $foreign["_singleton"](singletonFallback);

  var takeFallback = function takeFallback(n) {
    return function (v) {
      if (n < 1) {
        return "";
      }

      ;
      var v1 = uncons(v);

      if (v1 instanceof Data_Maybe.Just) {
        return singleton(v1.value0.head) + takeFallback(n - 1 | 0)(v1.value0.tail);
      }

      ;
      return v;
    };
  };

  var take = $foreign["_take"](takeFallback);

  var drop = function drop(n) {
    return function (s) {
      return Data_String_CodeUnits.drop(Data_String_CodeUnits.length(take(n)(s)))(s);
    };
  };

  exports["length"] = length;
  exports["indexOf"] = indexOf;
  exports["take"] = take;
  exports["drop"] = drop;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Text.Parsing.Parser.String"] = $PS["Text.Parsing.Parser.String"] || {};
  var exports = $PS["Text.Parsing.Parser.String"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_CodePoints = $PS["Data.String.CodePoints"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_String_Pattern = $PS["Data.String.Pattern"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_Pos = $PS["Text.Parsing.Parser.Pos"];

  var StringLike = function StringLike(drop, indexOf, $$null, uncons) {
    this.drop = drop;
    this.indexOf = indexOf;
    this["null"] = $$null;
    this.uncons = uncons;
  };

  var uncons = function uncons(dict) {
    return dict.uncons;
  };

  var stringLikeString = new StringLike(Data_String_CodePoints.drop, Data_String_CodePoints.indexOf, Data_String_Common["null"], Data_String_CodeUnits.uncons);

  var $$null = function $$null(dict) {
    return dict["null"];
  };

  var indexOf = function indexOf(dict) {
    return dict.indexOf;
  };

  var eof = function eof(dictStringLike) {
    return function (dictMonad) {
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
        return v.value0;
      }))(function (v) {
        return Control_Applicative.unless(Text_Parsing_Parser.applicativeParserT(dictMonad))($$null(dictStringLike)(v))(Text_Parsing_Parser.fail(dictMonad)("Expected EOF"));
      });
    };
  };

  var drop = function drop(dict) {
    return dict.drop;
  };

  var string = function string(dictStringLike) {
    return function (dictMonad) {
      return function (str) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
          return v.value0;
        }))(function (v) {
          var v1 = indexOf(dictStringLike)(Data_Newtype.wrap(Data_String_Pattern.newtypePattern)(str))(v);

          if (v1 instanceof Data_Maybe.Just && v1.value0 === 0) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.modify_(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v2) {
              return new Text_Parsing_Parser.ParseState(drop(dictStringLike)(Data_String_CodePoints.length(str))(v), Text_Parsing_Parser_Pos.updatePosString(v2.value1)(str), true);
            }))(function () {
              return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(str);
            });
          }

          ;
          return Text_Parsing_Parser.fail(dictMonad)("Expected " + Data_Show.show(Data_Show.showString)(str));
        });
      };
    };
  };

  var anyChar = function anyChar(dictStringLike) {
    return function (dictMonad) {
      return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.gets(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v) {
        return v.value0;
      }))(function (v) {
        var v1 = uncons(dictStringLike)(v);

        if (v1 instanceof Data_Maybe.Nothing) {
          return Text_Parsing_Parser.fail(dictMonad)("Unexpected EOF");
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Monad_State_Class.modify_(Text_Parsing_Parser.monadStateParserT(dictMonad))(function (v2) {
            return new Text_Parsing_Parser.ParseState(v1.value0.tail, Text_Parsing_Parser_Pos.updatePosString(v2.value1)(Data_String_CodeUnits.singleton(v1.value0.head)), true);
          }))(function () {
            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v1.value0.head);
          });
        }

        ;
        throw new Error("Failed pattern match at Text.Parsing.Parser.String (line 56, column 3 - line 63, column 16): " + [v1.constructor.name]);
      });
    };
  };

  var satisfy = function satisfy(dictStringLike) {
    return function (dictMonad) {
      return function (f) {
        return Text_Parsing_Parser_Combinators.tryRethrow(dictMonad)(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(anyChar(dictStringLike)(dictMonad))(function (v) {
          var $61 = f(v);

          if ($61) {
            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(v);
          }

          ;
          return Text_Parsing_Parser.fail(dictMonad)("Character '" + (Data_String_CodeUnits.singleton(v) + "' did not satisfy predicate"));
        }));
      };
    };
  };

  var noneOf = function noneOf(dictStringLike) {
    return function (dictMonad) {
      return function (ss) {
        return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(satisfy(dictStringLike)(dictMonad)(Data_Function.flip(Data_Foldable.notElem(Data_Foldable.foldableArray)(Data_Eq.eqChar))(ss)))("none of " + Data_Show.show(Data_Show.showArray(Data_Show.showChar))(ss));
      };
    };
  };

  exports["eof"] = eof;
  exports["string"] = string;
  exports["noneOf"] = noneOf;
  exports["stringLikeString"] = stringLikeString;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Formatter.Parser.Utils"] = $PS["Data.Formatter.Parser.Utils"] || {};
  var exports = $PS["Data.Formatter.Parser.Utils"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Show = $PS["Data.Show"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_String = $PS["Text.Parsing.Parser.String"];

  var printPosition = function printPosition(v) {
    return "(line " + (Data_Show.show(Data_Show.showInt)(v.line) + (", col " + (Data_Show.show(Data_Show.showInt)(v.column) + ")")));
  };

  var printError = function printError(err) {
    return Text_Parsing_Parser.parseErrorMessage(err) + (" " + printPosition(Text_Parsing_Parser.parseErrorPosition(err)));
  };

  var runP = function runP(dictStringLike) {
    return function (p) {
      return function (s) {
        return Data_Bifunctor.lmap(Data_Either.bifunctorEither)(printError)(Text_Parsing_Parser.runParser(s)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(p)(Text_Parsing_Parser_String.eof(dictStringLike)(Data_Identity.monadIdentity))));
      };
    };
  };

  var oneOfAs = function oneOfAs(dictFunctor) {
    return function (dictFoldable) {
      return function (dictMonad) {
        return function (p) {
          return function (xs) {
            return Text_Parsing_Parser_Combinators.choice(dictFoldable)(dictMonad)(Data_Functor.map(dictFunctor)(function (v) {
              return Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(dictMonad.Bind1().Apply0().Functor0()))(p(v.value0))(v.value1);
            })(xs));
          };
        };
      };
    };
  };

  exports["oneOfAs"] = oneOfAs;
  exports["runP"] = runP;
})(PS);

(function (exports) {
  "use strict";

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});

(function (exports) {
  /* globals exports */
  "use strict";

  exports.infinity = Infinity;
})(PS["Global"] = PS["Global"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Global"] = $PS["Global"] || {};
  var exports = $PS["Global"];
  var $foreign = $PS["Global"];
  exports["infinity"] = $foreign.infinity;
})(PS);

(function (exports) {
  "use strict";

  exports.floor = Math.floor;
})(PS["Math"] = PS["Math"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["floor"] = $foreign.floor;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Int"] = $PS["Data.Int"] || {};
  var exports = $PS["Data.Int"];
  var $foreign = $PS["Data.Int"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Global = $PS["Global"];
  var $$Math = $PS["Math"];
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var unsafeClamp = function unsafeClamp(x) {
    if (x === Global.infinity) {
      return 0;
    }

    ;

    if (x === -Global.infinity) {
      return 0;
    }

    ;

    if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
      return Data_Bounded.top(Data_Bounded.boundedInt);
    }

    ;

    if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
      return Data_Bounded.bottom(Data_Bounded.boundedInt);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.fromMaybe(0)(fromNumber(x));
    }

    ;
    throw new Error("Failed pattern match at Data.Int (line 66, column 1 - line 66, column 29): " + [x.constructor.name]);
  };

  var floor = function floor($24) {
    return unsafeClamp($$Math.floor($24));
  };

  exports["floor"] = floor;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Time.Component"] = $PS["Data.Time.Component"] || {};
  var exports = $PS["Data.Time.Component"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var ordSecond = Data_Ord.ordInt;
  var ordMinute = Data_Ord.ordInt;
  var ordMillisecond = Data_Ord.ordInt;
  var ordHour = Data_Ord.ordInt;
  var boundedSecond = new Data_Bounded.Bounded(function () {
    return ordSecond;
  }, 0, 59);
  var boundedMinute = new Data_Bounded.Bounded(function () {
    return ordMinute;
  }, 0, 59);
  var boundedMillisecond = new Data_Bounded.Bounded(function () {
    return ordMillisecond;
  }, 0, 999);
  var boundedHour = new Data_Bounded.Bounded(function () {
    return ordHour;
  }, 0, 23);
  var boundedEnumSecond = new Data_Enum.BoundedEnum(function () {
    return boundedSecond;
  }, function () {
    return enumSecond;
  }, 60, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 59) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
  });
  var enumSecond = new Data_Enum.Enum(function () {
    return ordSecond;
  }, function () {
    var $28 = Data_Enum.toEnum(boundedEnumSecond);
    var $29 = Data_Enum.fromEnum(boundedEnumSecond);
    return function ($30) {
      return $28(function (v) {
        return v - 1 | 0;
      }($29($30)));
    };
  }(), function () {
    var $31 = Data_Enum.toEnum(boundedEnumSecond);
    var $32 = Data_Enum.fromEnum(boundedEnumSecond);
    return function ($33) {
      return $31(function (v) {
        return v + 1 | 0;
      }($32($33)));
    };
  }());
  var boundedEnumMinute = new Data_Enum.BoundedEnum(function () {
    return boundedMinute;
  }, function () {
    return enumMinute;
  }, 60, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 59) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
  });
  var enumMinute = new Data_Enum.Enum(function () {
    return ordMinute;
  }, function () {
    var $34 = Data_Enum.toEnum(boundedEnumMinute);
    var $35 = Data_Enum.fromEnum(boundedEnumMinute);
    return function ($36) {
      return $34(function (v) {
        return v - 1 | 0;
      }($35($36)));
    };
  }(), function () {
    var $37 = Data_Enum.toEnum(boundedEnumMinute);
    var $38 = Data_Enum.fromEnum(boundedEnumMinute);
    return function ($39) {
      return $37(function (v) {
        return v + 1 | 0;
      }($38($39)));
    };
  }());
  var boundedEnumMillisecond = new Data_Enum.BoundedEnum(function () {
    return boundedMillisecond;
  }, function () {
    return enumMillisecond;
  }, 1000, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 999) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
  });
  var enumMillisecond = new Data_Enum.Enum(function () {
    return ordMillisecond;
  }, function () {
    var $40 = Data_Enum.toEnum(boundedEnumMillisecond);
    var $41 = Data_Enum.fromEnum(boundedEnumMillisecond);
    return function ($42) {
      return $40(function (v) {
        return v - 1 | 0;
      }($41($42)));
    };
  }(), function () {
    var $43 = Data_Enum.toEnum(boundedEnumMillisecond);
    var $44 = Data_Enum.fromEnum(boundedEnumMillisecond);
    return function ($45) {
      return $43(function (v) {
        return v + 1 | 0;
      }($44($45)));
    };
  }());
  var boundedEnumHour = new Data_Enum.BoundedEnum(function () {
    return boundedHour;
  }, function () {
    return enumHour;
  }, 24, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 23) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
  });
  var enumHour = new Data_Enum.Enum(function () {
    return ordHour;
  }, function () {
    var $46 = Data_Enum.toEnum(boundedEnumHour);
    var $47 = Data_Enum.fromEnum(boundedEnumHour);
    return function ($48) {
      return $46(function (v) {
        return v - 1 | 0;
      }($47($48)));
    };
  }(), function () {
    var $49 = Data_Enum.toEnum(boundedEnumHour);
    var $50 = Data_Enum.fromEnum(boundedEnumHour);
    return function ($51) {
      return $49(function (v) {
        return v + 1 | 0;
      }($50($51)));
    };
  }());
  exports["boundedEnumHour"] = boundedEnumHour;
  exports["boundedEnumMinute"] = boundedEnumMinute;
  exports["boundedEnumSecond"] = boundedEnumSecond;
  exports["boundedEnumMillisecond"] = boundedEnumMillisecond;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Time.Duration"] = $PS["Data.Time.Duration"] || {};
  var exports = $PS["Data.Time.Duration"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Milliseconds = function Milliseconds(x) {
    return x;
  };

  var newtypeMilliseconds = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Milliseconds);
  exports["newtypeMilliseconds"] = newtypeMilliseconds;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Formatter.DateTime"] = $PS["Data.Formatter.DateTime"] || {};
  var exports = $PS["Data.Formatter.DateTime"];
  var Control_Alt = $PS["Control.Alt"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_DateTime_Instant = $PS["Data.DateTime.Instant"];
  var Data_Either = $PS["Data.Either"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Formatter_Parser_Utils = $PS["Data.Formatter.Parser.Utils"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Int = $PS["Data.Int"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_CodePoints = $PS["Data.String.CodePoints"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_Time = $PS["Data.Time"];
  var Data_Time_Component = $PS["Data.Time.Component"];
  var Data_Time_Duration = $PS["Data.Time.Duration"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Text_Parsing_Parser = $PS["Text.Parsing.Parser"];
  var Text_Parsing_Parser_Combinators = $PS["Text.Parsing.Parser.Combinators"];
  var Text_Parsing_Parser_String = $PS["Text.Parsing.Parser.String"];

  var YearFull = function () {
    function YearFull() {}

    ;
    YearFull.value = new YearFull();
    return YearFull;
  }();

  var YearTwoDigits = function () {
    function YearTwoDigits() {}

    ;
    YearTwoDigits.value = new YearTwoDigits();
    return YearTwoDigits;
  }();

  var YearAbsolute = function () {
    function YearAbsolute() {}

    ;
    YearAbsolute.value = new YearAbsolute();
    return YearAbsolute;
  }();

  var MonthFull = function () {
    function MonthFull() {}

    ;
    MonthFull.value = new MonthFull();
    return MonthFull;
  }();

  var MonthShort = function () {
    function MonthShort() {}

    ;
    MonthShort.value = new MonthShort();
    return MonthShort;
  }();

  var MonthTwoDigits = function () {
    function MonthTwoDigits() {}

    ;
    MonthTwoDigits.value = new MonthTwoDigits();
    return MonthTwoDigits;
  }();

  var DayOfMonthTwoDigits = function () {
    function DayOfMonthTwoDigits() {}

    ;
    DayOfMonthTwoDigits.value = new DayOfMonthTwoDigits();
    return DayOfMonthTwoDigits;
  }();

  var DayOfMonth = function () {
    function DayOfMonth() {}

    ;
    DayOfMonth.value = new DayOfMonth();
    return DayOfMonth;
  }();

  var UnixTimestamp = function () {
    function UnixTimestamp() {}

    ;
    UnixTimestamp.value = new UnixTimestamp();
    return UnixTimestamp;
  }();

  var DayOfWeek = function () {
    function DayOfWeek() {}

    ;
    DayOfWeek.value = new DayOfWeek();
    return DayOfWeek;
  }();

  var DayOfWeekName = function () {
    function DayOfWeekName() {}

    ;
    DayOfWeekName.value = new DayOfWeekName();
    return DayOfWeekName;
  }();

  var DayOfWeekNameShort = function () {
    function DayOfWeekNameShort() {}

    ;
    DayOfWeekNameShort.value = new DayOfWeekNameShort();
    return DayOfWeekNameShort;
  }();

  var Hours24 = function () {
    function Hours24() {}

    ;
    Hours24.value = new Hours24();
    return Hours24;
  }();

  var Hours12 = function () {
    function Hours12() {}

    ;
    Hours12.value = new Hours12();
    return Hours12;
  }();

  var Meridiem = function () {
    function Meridiem() {}

    ;
    Meridiem.value = new Meridiem();
    return Meridiem;
  }();

  var Minutes = function () {
    function Minutes() {}

    ;
    Minutes.value = new Minutes();
    return Minutes;
  }();

  var MinutesTwoDigits = function () {
    function MinutesTwoDigits() {}

    ;
    MinutesTwoDigits.value = new MinutesTwoDigits();
    return MinutesTwoDigits;
  }();

  var Seconds = function () {
    function Seconds() {}

    ;
    Seconds.value = new Seconds();
    return Seconds;
  }();

  var SecondsTwoDigits = function () {
    function SecondsTwoDigits() {}

    ;
    SecondsTwoDigits.value = new SecondsTwoDigits();
    return SecondsTwoDigits;
  }();

  var Milliseconds = function () {
    function Milliseconds() {}

    ;
    Milliseconds.value = new Milliseconds();
    return Milliseconds;
  }();

  var MillisecondsShort = function () {
    function MillisecondsShort() {}

    ;
    MillisecondsShort.value = new MillisecondsShort();
    return MillisecondsShort;
  }();

  var MillisecondsTwoDigits = function () {
    function MillisecondsTwoDigits() {}

    ;
    MillisecondsTwoDigits.value = new MillisecondsTwoDigits();
    return MillisecondsTwoDigits;
  }();

  var Placeholder = function () {
    function Placeholder(value0) {
      this.value0 = value0;
    }

    ;

    Placeholder.create = function (value0) {
      return new Placeholder(value0);
    };

    return Placeholder;
  }();

  var printShortMonth = function printShortMonth(v) {
    if (v instanceof Data_Date_Component.January) {
      return "Jan";
    }

    ;

    if (v instanceof Data_Date_Component.February) {
      return "Feb";
    }

    ;

    if (v instanceof Data_Date_Component.March) {
      return "Mar";
    }

    ;

    if (v instanceof Data_Date_Component.April) {
      return "Apr";
    }

    ;

    if (v instanceof Data_Date_Component.May) {
      return "May";
    }

    ;

    if (v instanceof Data_Date_Component.June) {
      return "Jun";
    }

    ;

    if (v instanceof Data_Date_Component.July) {
      return "Jul";
    }

    ;

    if (v instanceof Data_Date_Component.August) {
      return "Aug";
    }

    ;

    if (v instanceof Data_Date_Component.September) {
      return "Sep";
    }

    ;

    if (v instanceof Data_Date_Component.October) {
      return "Oct";
    }

    ;

    if (v instanceof Data_Date_Component.November) {
      return "Nov";
    }

    ;

    if (v instanceof Data_Date_Component.December) {
      return "Dec";
    }

    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 482, column 19 - line 494, column 21): " + [v.constructor.name]);
  };

  var placeholderContent = Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_Array.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Text_Parsing_Parser_String.noneOf(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(Data_String_CodeUnits.toCharArray("YMDEHhamsS"))))(Data_String_CodeUnits.fromCharArray);

  var padSingleDigit = function padSingleDigit(i) {
    if (i < 0) {
      return "-" + padSingleDigit(-i | 0);
    }

    ;

    if (i < 10) {
      return "0" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Show.show(Data_Show.showInt)(i);
    }

    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 192, column 1 - line 192, column 30): " + [i.constructor.name]);
  };

  var padQuadrupleDigit = function padQuadrupleDigit(i) {
    if (i < 0) {
      return "-" + padQuadrupleDigit(-i | 0);
    }

    ;

    if (i < 10) {
      return "000" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (i < 100) {
      return "00" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (i < 1000) {
      return "0" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Show.show(Data_Show.showInt)(i);
    }

    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 205, column 1 - line 205, column 33): " + [i.constructor.name]);
  };

  var padDoubleDigit = function padDoubleDigit(i) {
    if (i < 0) {
      return "-" + padDoubleDigit(-i | 0);
    }

    ;

    if (i < 10) {
      return "00" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (i < 100) {
      return "0" + Data_Show.show(Data_Show.showInt)(i);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Show.show(Data_Show.showInt)(i);
    }

    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 198, column 1 - line 198, column 30): " + [i.constructor.name]);
  };

  var formatterCommandParser = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Formatter_Parser_Utils.oneOfAs(Data_Functor.functorArray)(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)(function () {
    var $460 = Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity);
    var $461 = Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity);
    return function ($462) {
      return $460($461($462));
    };
  }())([new Data_Tuple.Tuple("YYYY", YearFull.value), new Data_Tuple.Tuple("YY", YearTwoDigits.value), new Data_Tuple.Tuple("Y", YearAbsolute.value), new Data_Tuple.Tuple("MMMM", MonthFull.value), new Data_Tuple.Tuple("MMM", MonthShort.value), new Data_Tuple.Tuple("MM", MonthTwoDigits.value), new Data_Tuple.Tuple("DD", DayOfMonthTwoDigits.value), new Data_Tuple.Tuple("D", DayOfMonth.value), new Data_Tuple.Tuple("E", DayOfWeek.value), new Data_Tuple.Tuple("X", UnixTimestamp.value), new Data_Tuple.Tuple("dddd", DayOfWeekName.value), new Data_Tuple.Tuple("ddd", DayOfWeekNameShort.value), new Data_Tuple.Tuple("HH", Hours24.value), new Data_Tuple.Tuple("hh", Hours12.value), new Data_Tuple.Tuple("a", Meridiem.value), new Data_Tuple.Tuple("mm", MinutesTwoDigits.value), new Data_Tuple.Tuple("m", Minutes.value), new Data_Tuple.Tuple("ss", SecondsTwoDigits.value), new Data_Tuple.Tuple("s", Seconds.value), new Data_Tuple.Tuple("SSS", Milliseconds.value), new Data_Tuple.Tuple("SS", MillisecondsTwoDigits.value), new Data_Tuple.Tuple("S", MillisecondsShort.value)]))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Placeholder.create)(placeholderContent));

  var formatYearTwoDigits = function formatYearTwoDigits(i) {
    var dateString = Data_Show.show(Data_Show.showInt)(Data_Ord.abs(Data_Ord.ordInt)(Data_Ring.ringInt)(i));
    var dateLength = Data_String_CodePoints.length(dateString);

    if (dateLength === 1) {
      return "0" + dateString;
    }

    ;

    if (dateLength === 2) {
      return dateString;
    }

    ;
    return Data_String_CodePoints.drop(dateLength - 2 | 0)(dateString);
  };

  var formatParser = Data_List.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(formatterCommandParser);
  var parseFormatString = Data_Formatter_Parser_Utils.runP(Text_Parsing_Parser_String.stringLikeString)(formatParser);

  var fix12 = function fix12(h) {
    var $401 = h === 0;

    if ($401) {
      return 12;
    }

    ;
    return h;
  };

  var formatCommand = function formatCommand(v) {
    return function (v1) {
      if (v1 instanceof YearFull) {
        return padQuadrupleDigit(Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0)));
      }

      ;

      if (v1 instanceof YearTwoDigits) {
        return formatYearTwoDigits(Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0)));
      }

      ;

      if (v1 instanceof YearAbsolute) {
        return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0)));
      }

      ;

      if (v1 instanceof MonthFull) {
        return Data_Show.show(Data_Date_Component.showMonth)(Data_Date.month(v.value0));
      }

      ;

      if (v1 instanceof MonthShort) {
        return printShortMonth(Data_Date.month(v.value0));
      }

      ;

      if (v1 instanceof MonthTwoDigits) {
        return padSingleDigit(Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)));
      }

      ;

      if (v1 instanceof DayOfMonthTwoDigits) {
        return padSingleDigit(Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(Data_Date.day(v.value0)));
      }

      ;

      if (v1 instanceof DayOfMonth) {
        return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(Data_Date.day(v.value0)));
      }

      ;

      if (v1 instanceof UnixTimestamp) {
        return Data_Show.show(Data_Show.showInt)(Data_Int.floor(function (v2) {
          return v2 / 1000.0;
        }(Data_Newtype.unwrap(Data_Time_Duration.newtypeMilliseconds)(Data_DateTime_Instant.unInstant(Data_DateTime_Instant.fromDateTime(v))))));
      }

      ;

      if (v1 instanceof DayOfWeek) {
        return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(Data_Date_Component.boundedEnumWeekday)(Data_Date.weekday(v.value0)));
      }

      ;

      if (v1 instanceof DayOfWeekName) {
        return Data_Show.show(Data_Date_Component.showWeekday)(Data_Date.weekday(v.value0));
      }

      ;

      if (v1 instanceof DayOfWeekNameShort) {
        return Data_String_CodePoints.take(3)(Data_Show.show(Data_Date_Component.showWeekday)(Data_Date.weekday(v.value0)));
      }

      ;

      if (v1 instanceof Hours24) {
        return padSingleDigit(Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1)));
      }

      ;

      if (v1 instanceof Hours12) {
        return padSingleDigit(fix12(Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1)))(12)));
      }

      ;

      if (v1 instanceof Meridiem) {
        var $404 = Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1)) >= 12;

        if ($404) {
          return "PM";
        }

        ;
        return "AM";
      }

      ;

      if (v1 instanceof Minutes) {
        return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(Data_Time.minute(v.value1)));
      }

      ;

      if (v1 instanceof MinutesTwoDigits) {
        return padSingleDigit(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(Data_Time.minute(v.value1)));
      }

      ;

      if (v1 instanceof Seconds) {
        return Data_Show.show(Data_Show.showInt)(Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(Data_Time.second(v.value1)));
      }

      ;

      if (v1 instanceof SecondsTwoDigits) {
        return padSingleDigit(Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(Data_Time.second(v.value1)));
      }

      ;

      if (v1 instanceof Milliseconds) {
        return padDoubleDigit(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1)));
      }

      ;

      if (v1 instanceof MillisecondsShort) {
        return Data_Show.show(Data_Show.showInt)(function (v2) {
          return Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(v2)(100);
        }(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1))));
      }

      ;

      if (v1 instanceof MillisecondsTwoDigits) {
        return padSingleDigit(function (v2) {
          return Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(v2)(10);
        }(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1))));
      }

      ;

      if (v1 instanceof Placeholder) {
        return v1.value0;
      }

      ;
      throw new Error("Failed pattern match at Data.Formatter.DateTime (line 167, column 38 - line 190, column 20): " + [v1.constructor.name]);
    };
  };

  var format = function format(f) {
    return function (d) {
      return Data_Foldable.foldMap(Data_List_Types.foldableList)(Data_Monoid.monoidString)(formatCommand(d))(f);
    };
  };

  var formatDateTime = function formatDateTime(pattern) {
    return function (datetime) {
      return Data_Functor.mapFlipped(Data_Either.functorEither)(parseFormatString(pattern))(function (v) {
        return format(v)(datetime);
      });
    };
  };

  exports["formatDateTime"] = formatDateTime;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Functor.Variant"] = $PS["Data.Functor.Variant"] || {};
  var exports = $PS["Data.Functor.Variant"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Partial_Unsafe = $PS["Partial.Unsafe"];
  var Record_Unsafe = $PS["Record.Unsafe"];

  var onMatch = function onMatch(dictRowToList) {
    return function (dictVariantFMatchCases) {
      return function (dictUnion) {
        return function (r) {
          return function (k) {
            return function (v) {
              if (Record_Unsafe.unsafeHas(v.type)(r)) {
                return Record_Unsafe.unsafeGet(v.type)(r)(v.value);
              }

              ;
              return k(v);
            };
          };
        };
      };
    };
  };

  var inj = function inj(dictCons) {
    return function (dictIsSymbol) {
      return function (dictFunctor) {
        return function (p) {
          return function (value) {
            return {
              type: Data_Symbol.reflectSymbol(dictIsSymbol)(p),
              value: value,
              map: Data_Functor.map(dictFunctor)
            };
          };
        };
      };
    };
  };

  var case_ = function case_(r) {
    return Partial_Unsafe.unsafeCrashWith("Data.Functor.Variant: pattern match failure [" + (r.type + "]"));
  };

  var match = function match(dictRowToList) {
    return function (dictVariantFMatchCases) {
      return function (dictUnion) {
        return function (r) {
          return onMatch()()()(r)(case_);
        };
      };
    };
  };

  exports["inj"] = inj;
  exports["match"] = match;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Generic.Rep"] = $PS["Data.Generic.Rep"] || {};
  var exports = $PS["Data.Generic.Rep"];

  var Inl = function () {
    function Inl(value0) {
      this.value0 = value0;
    }

    ;

    Inl.create = function (value0) {
      return new Inl(value0);
    };

    return Inl;
  }();

  var Inr = function () {
    function Inr(value0) {
      this.value0 = value0;
    }

    ;

    Inr.create = function (value0) {
      return new Inr(value0);
    };

    return Inr;
  }();

  var NoArguments = function () {
    function NoArguments() {}

    ;
    NoArguments.value = new NoArguments();
    return NoArguments;
  }();

  var Constructor = function Constructor(x) {
    return x;
  };

  var Argument = function Argument(x) {
    return x;
  };

  var Generic = function Generic(from, to) {
    this.from = from;
    this.to = to;
  };

  var to = function to(dict) {
    return dict.to;
  };

  var from = function from(dict) {
    return dict.from;
  };

  exports["Generic"] = Generic;
  exports["to"] = to;
  exports["from"] = from;
  exports["NoArguments"] = NoArguments;
  exports["Inl"] = Inl;
  exports["Inr"] = Inr;
  exports["Constructor"] = Constructor;
  exports["Argument"] = Argument;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Internal.Wander"] = $PS["Data.Lens.Internal.Wander"] || {};
  var exports = $PS["Data.Lens.Internal.Wander"];

  var Wander = function Wander(Choice1, Strong0, wander) {
    this.Choice1 = Choice1;
    this.Strong0 = Strong0;
    this.wander = wander;
  };

  exports["Wander"] = Wander;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Profunctor.Choice"] = $PS["Data.Profunctor.Choice"] || {};
  var exports = $PS["Data.Profunctor.Choice"];

  var Choice = function Choice(Profunctor0, left, right) {
    this.Profunctor0 = Profunctor0;
    this.left = left;
    this.right = right;
  };

  var right = function right(dict) {
    return dict.right;
  };

  exports["right"] = right;
  exports["Choice"] = Choice;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Profunctor.Strong"] = $PS["Data.Profunctor.Strong"] || {};
  var exports = $PS["Data.Profunctor.Strong"];

  var Strong = function Strong(Profunctor0, first, second) {
    this.Profunctor0 = Profunctor0;
    this.first = first;
    this.second = second;
  };

  var first = function first(dict) {
    return dict.first;
  };

  exports["first"] = first;
  exports["Strong"] = Strong;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Internal.Forget"] = $PS["Data.Lens.Internal.Forget"] || {};
  var exports = $PS["Data.Lens.Internal.Forget"];
  var Data_Const = $PS["Data.Const"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Lens_Internal_Wander = $PS["Data.Lens.Internal.Wander"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Profunctor_Choice = $PS["Data.Profunctor.Choice"];
  var Data_Profunctor_Strong = $PS["Data.Profunctor.Strong"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Forget = function Forget(x) {
    return x;
  };

  var profunctorForget = new Data_Profunctor.Profunctor(function (f) {
    return function (v) {
      return function (v1) {
        return function ($27) {
          return v1(f($27));
        };
      };
    };
  });
  var strongForget = new Data_Profunctor_Strong.Strong(function () {
    return profunctorForget;
  }, function (v) {
    return function ($28) {
      return v(Data_Tuple.fst($28));
    };
  }, function (v) {
    return function ($29) {
      return v(Data_Tuple.snd($29));
    };
  });
  var newtypeForget = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Forget);

  var choiceForget = function choiceForget(dictMonoid) {
    return new Data_Profunctor_Choice.Choice(function () {
      return profunctorForget;
    }, function (v) {
      return Data_Either.either(v)(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)));
    }, function (v) {
      return Data_Either.either(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)))(v);
    });
  };

  var wanderForget = function wanderForget(dictMonoid) {
    return new Data_Lens_Internal_Wander.Wander(function () {
      return choiceForget(dictMonoid);
    }, function () {
      return strongForget;
    }, function (f) {
      return function (v) {
        return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Const.newtypeConst)(Data_Const.newtypeConst)(Data_Const.Const)(f(Data_Const.applicativeConst(dictMonoid)))(v);
      };
    });
  };

  exports["Forget"] = Forget;
  exports["newtypeForget"] = newtypeForget;
  exports["strongForget"] = strongForget;
  exports["wanderForget"] = wanderForget;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Maybe.First"] = $PS["Data.Maybe.First"] || {};
  var exports = $PS["Data.Maybe.First"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var First = function First(x) {
    return x;
  };

  var semigroupFirst = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
      if (v instanceof Data_Maybe.Just) {
        return v;
      }

      ;
      return v1;
    };
  });
  var newtypeFirst = new Data_Newtype.Newtype(function (n) {
    return n;
  }, First);
  var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
  }, Data_Maybe.Nothing.value);
  exports["First"] = First;
  exports["newtypeFirst"] = newtypeFirst;
  exports["monoidFirst"] = monoidFirst;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Fold"] = $PS["Data.Lens.Fold"] || {};
  var exports = $PS["Data.Lens.Fold"];
  var Data_Lens_Internal_Forget = $PS["Data.Lens.Internal.Forget"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Maybe_First = $PS["Data.Maybe.First"];
  var Data_Newtype = $PS["Data.Newtype"];
  var foldMapOf = Data_Newtype.under(Data_Lens_Internal_Forget.newtypeForget)(Data_Lens_Internal_Forget.newtypeForget)(Data_Lens_Internal_Forget.Forget);

  var preview = function preview(p) {
    var $95 = Data_Newtype.unwrap(Data_Maybe_First.newtypeFirst);
    var $96 = foldMapOf(p)(function ($98) {
      return Data_Maybe_First.First(Data_Maybe.Just.create($98));
    });
    return function ($97) {
      return $95($96($97));
    };
  };

  exports["preview"] = preview;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Getter"] = $PS["Data.Lens.Getter"] || {};
  var exports = $PS["Data.Lens.Getter"];
  var Control_Category = $PS["Control.Category"];
  var Data_Lens_Internal_Forget = $PS["Data.Lens.Internal.Forget"];
  var Data_Newtype = $PS["Data.Newtype"];

  var view = function view(l) {
    return Data_Newtype.unwrap(Data_Lens_Internal_Forget.newtypeForget)(l(Control_Category.identity(Control_Category.categoryFn)));
  };

  exports["view"] = view;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Iso"] = $PS["Data.Lens.Iso"] || {};
  var exports = $PS["Data.Lens.Iso"];
  var Data_Profunctor = $PS["Data.Profunctor"];

  var iso = function iso(f) {
    return function (g) {
      return function (dictProfunctor) {
        return function (pab) {
          return Data_Profunctor.dimap(dictProfunctor)(f)(g)(pab);
        };
      };
    };
  };

  exports["iso"] = iso;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Iso.Newtype"] = $PS["Data.Lens.Iso.Newtype"] || {};
  var exports = $PS["Data.Lens.Iso.Newtype"];
  var Data_Lens_Iso = $PS["Data.Lens.Iso"];
  var Data_Newtype = $PS["Data.Newtype"];

  var _Newtype = function _Newtype(dictNewtype) {
    return function (dictNewtype1) {
      return function (dictProfunctor) {
        return Data_Lens_Iso.iso(Data_Newtype.unwrap(dictNewtype))(Data_Newtype.wrap(dictNewtype1))(dictProfunctor);
      };
    };
  };

  exports["_Newtype"] = _Newtype;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Lens"] = $PS["Data.Lens.Lens"] || {};
  var exports = $PS["Data.Lens.Lens"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Profunctor_Strong = $PS["Data.Profunctor.Strong"];
  var Data_Tuple = $PS["Data.Tuple"];

  var lens$prime = function lens$prime(to) {
    return function (dictStrong) {
      return function (pab) {
        return Data_Profunctor.dimap(dictStrong.Profunctor0())(to)(function (v) {
          return v.value1(v.value0);
        })(Data_Profunctor_Strong.first(dictStrong)(pab));
      };
    };
  };

  var lens = function lens(get) {
    return function (set) {
      return function (dictStrong) {
        return lens$prime(function (s) {
          return new Data_Tuple.Tuple(get(s), function (b) {
            return set(s)(b);
          });
        })(dictStrong);
      };
    };
  };

  exports["lens"] = lens;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Prism"] = $PS["Data.Lens.Prism"] || {};
  var exports = $PS["Data.Lens.Prism"];
  var Control_Category = $PS["Control.Category"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Profunctor_Choice = $PS["Data.Profunctor.Choice"];

  var prism = function prism(to) {
    return function (fro) {
      return function (dictChoice) {
        return function (pab) {
          return Data_Profunctor.dimap(dictChoice.Profunctor0())(fro)(Data_Either.either(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn)))(Data_Profunctor_Choice.right(dictChoice)(Data_Profunctor.rmap(dictChoice.Profunctor0())(to)(pab)));
        };
      };
    };
  };

  var prism$prime = function prism$prime(to) {
    return function (fro) {
      return function (dictChoice) {
        return prism(to)(function (s) {
          return Data_Maybe.maybe(new Data_Either.Left(s))(Data_Either.Right.create)(fro(s));
        })(dictChoice);
      };
    };
  };

  exports["prism'"] = prism$prime;
  exports["prism"] = prism;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Lens.Record"] = $PS["Data.Lens.Record"] || {};
  var exports = $PS["Data.Lens.Record"];
  var Data_Function = $PS["Data.Function"];
  var Data_Lens_Lens = $PS["Data.Lens.Lens"];
  var Record = $PS["Record"];

  var prop = function prop(dictIsSymbol) {
    return function (dictCons) {
      return function (dictCons1) {
        return function (l) {
          return function (dictStrong) {
            return Data_Lens_Lens.lens(Record.get(dictIsSymbol)()(l))(Data_Function.flip(Record.set(dictIsSymbol)()()(l)))(dictStrong);
          };
        };
      };
    };
  };

  exports["prop"] = prop;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeRenameManyFunction = function (tuples, record) {
    var result = {};

    for (var key in record) {
      if ({}.hasOwnProperty.call(record, key)) {
        result[key] = record[key];
      }
    }

    var count = tuples.length;

    for (var i = 0; i < count; i++) {
      var tuple = tuples[i];
      var oldKey = tuple.value0;
      var newKey = tuple.value1;
      result[newKey] = result[oldKey];
      delete result[oldKey];
    }

    return result;
  };
})(PS["Data.Struct.RenameMany.RRenameMany"] = PS["Data.Struct.RenameMany.RRenameMany"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Struct.Utils.ReifyKeyAndValueSymbols"] = $PS["Data.Struct.Utils.ReifyKeyAndValueSymbols"] || {};
  var exports = $PS["Data.Struct.Utils.ReifyKeyAndValueSymbols"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var ReifyKeyAndValueSymbols = function ReifyKeyAndValueSymbols(reifyKeyAndValueSymbols$prime) {
    this["reifyKeyAndValueSymbols'"] = reifyKeyAndValueSymbols$prime;
  };

  var reifyKeyAndValueSymbolsNil = new ReifyKeyAndValueSymbols(Data_Monoid.mempty(Data_Monoid.monoidFn(Data_List_Types.monoidList)));

  var reifyKeyAndValueSymbols$prime = function reifyKeyAndValueSymbols$prime(dict) {
    return dict["reifyKeyAndValueSymbols'"];
  };

  var reifyKeyAndValueSymbolsCons = function reifyKeyAndValueSymbolsCons(dictIsSymbol) {
    return function (dictIsSymbol1) {
      return function (dictReifyKeyAndValueSymbols) {
        return new ReifyKeyAndValueSymbols(function (v) {
          var string1 = Data_Symbol.reflectSymbol(dictIsSymbol1)(Data_Symbol.SProxy.value);
          var string0 = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
          var rest = reifyKeyAndValueSymbols$prime(dictReifyKeyAndValueSymbols)(Type_Data_RowList.RLProxy.value);
          return new Data_List_Types.Cons(new Data_Tuple.Tuple(string0, string1), rest);
        });
      };
    };
  };

  var reifyKeyAndValueSymbols = function reifyKeyAndValueSymbols(dictReifyKeyAndValueSymbols) {
    return function (dictRowToList) {
      return function (v) {
        return reifyKeyAndValueSymbols$prime(dictReifyKeyAndValueSymbols)(Type_Data_RowList.RLProxy.value);
      };
    };
  };

  exports["reifyKeyAndValueSymbols"] = reifyKeyAndValueSymbols;
  exports["reifyKeyAndValueSymbolsNil"] = reifyKeyAndValueSymbolsNil;
  exports["reifyKeyAndValueSymbolsCons"] = reifyKeyAndValueSymbolsCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Struct.RenameMany.RRenameMany"] = $PS["Data.Struct.RenameMany.RRenameMany"] || {};
  var exports = $PS["Data.Struct.RenameMany.RRenameMany"];
  var $foreign = $PS["Data.Struct.RenameMany.RRenameMany"];
  var Data_Array = $PS["Data.Array"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Struct_Utils_ReifyKeyAndValueSymbols = $PS["Data.Struct.Utils.ReifyKeyAndValueSymbols"];

  var RRenameMany = function RRenameMany(rrenameMany) {
    this.rrenameMany = rrenameMany;
  };

  var rrenameManyFunction = function rrenameManyFunction(dictListToRow) {
    return function (dictReifyKeyAndValueSymbols) {
      return function (dictRowToList) {
        return function (dictRowToList1) {
          return new RRenameMany(function (dictGRenameMany) {
            return function (nameChanges) {
              return function (record) {
                var nameChanges$prime = Data_Array.fromFoldable(Data_List_Types.foldableList)(Data_Struct_Utils_ReifyKeyAndValueSymbols.reifyKeyAndValueSymbols(dictReifyKeyAndValueSymbols)()(nameChanges));
                return $foreign.unsafeRenameManyFunction(nameChanges$prime, record);
              };
            };
          });
        };
      };
    };
  };

  var rrenameMany = function rrenameMany(dict) {
    return dict.rrenameMany;
  };

  exports["rrenameMany"] = rrenameMany;
  exports["rrenameManyFunction"] = rrenameManyFunction;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Variant"] = $PS["Data.Variant"] || {};
  var exports = $PS["Data.Variant"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Partial_Unsafe = $PS["Partial.Unsafe"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var onMatch = function onMatch(dictRowToList) {
    return function (dictVariantMatchCases) {
      return function (dictUnion) {
        return function (r) {
          return function (k) {
            return function (v) {
              if (Record_Unsafe.unsafeHas(v.type)(r)) {
                return Record_Unsafe.unsafeGet(v.type)(r)(v.value);
              }

              ;
              return k(v);
            };
          };
        };
      };
    };
  };

  var inj = function inj(dictCons) {
    return function (dictIsSymbol) {
      return function (p) {
        return function (value) {
          return {
            type: Data_Symbol.reflectSymbol(dictIsSymbol)(p),
            value: value
          };
        };
      };
    };
  };

  var expand = function expand(dictUnion) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var case_ = function case_(r) {
    return Partial_Unsafe.unsafeCrashWith("Data.Variant: pattern match failure [" + (r.type + "]"));
  };

  var match = function match(dictRowToList) {
    return function (dictVariantMatchCases) {
      return function (dictUnion) {
        return function (r) {
          return onMatch()()()(r)(case_);
        };
      };
    };
  };

  exports["inj"] = inj;
  exports["match"] = match;
  exports["expand"] = expand;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Data.Void"] = $PS["Data.Void"] || {};
  var exports = $PS["Data.Void"];

  var absurd = function absurd(a) {
    var spin = function spin($copy_v) {
      var $tco_result;

      function $tco_loop(v) {
        $copy_v = v;
        return;
      }

      ;

      while (!false) {
        $tco_result = $tco_loop($copy_v);
      }

      ;
      return $tco_result;
    };

    return spin(a);
  };

  exports["absurd"] = absurd;
})(PS);

(function (exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };

  exports.warn = function (s) {
    return function () {
      console.warn(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  exports["log"] = $foreign.log;
  exports["warn"] = $foreign.warn;
})(PS);

(function (exports) {
  "use strict";

  exports.now = function () {
    return Date.now();
  };
})(PS["Effect.Now"] = PS["Effect.Now"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Effect.Now"] = $PS["Effect.Now"] || {};
  var exports = $PS["Effect.Now"];
  var $foreign = $PS["Effect.Now"];
  var Data_DateTime = $PS["Data.DateTime"];
  var Data_DateTime_Instant = $PS["Data.DateTime.Instant"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect = $PS["Effect"];
  var nowTime = Data_Functor.map(Effect.functorEffect)(function ($0) {
    return Data_DateTime.time(Data_DateTime_Instant.toDateTime($0));
  })($foreign.now);
  var nowDateTime = Data_Functor.map(Effect.functorEffect)(Data_DateTime_Instant.toDateTime)($foreign.now);
  var nowDate = Data_Functor.map(Effect.functorEffect)(function ($1) {
    return Data_DateTime.date(Data_DateTime_Instant.toDateTime($1));
  })($foreign.now);
  exports["nowDateTime"] = nowDateTime;
  exports["nowDate"] = nowDate;
  exports["nowTime"] = nowTime;
  exports["now"] = $foreign.now;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeReadPropImpl = function (f, s, key, value) {
    return value == null ? f : s(value[key]);
  };
})(PS["Foreign.Index"] = PS["Foreign.Index"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Foreign.Index"] = $PS["Foreign.Index"] || {};
  var exports = $PS["Foreign.Index"];
  var $foreign = $PS["Foreign.Index"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Identity = $PS["Data.Identity"];
  var Foreign = $PS["Foreign"];

  var unsafeReadProp = function unsafeReadProp(k) {
    return function (value) {
      return $foreign.unsafeReadPropImpl(Foreign.fail(new Foreign.TypeMismatch("object", Foreign.typeOf(value))), Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity)), k, value);
    };
  };

  var readProp = unsafeReadProp;
  exports["readProp"] = readProp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Types.Form"] = $PS["Formless.Types.Form"] || {};
  var exports = $PS["Formless.Types.Form"];
  var Data_Newtype = $PS["Data.Newtype"];

  var OutputField = function OutputField(x) {
    return x;
  };

  var InputFunction = function InputFunction(x) {
    return x;
  };

  var InputField = function InputField(x) {
    return x;
  };

  var FormProxy = function () {
    function FormProxy() {}

    ;
    FormProxy.value = new FormProxy();
    return FormProxy;
  }();

  var FormField = function FormField(x) {
    return x;
  };

  var newtypeOutputField = new Data_Newtype.Newtype(function (n) {
    return n;
  }, OutputField);
  var newtypeInputFunction = new Data_Newtype.Newtype(function (n) {
    return n;
  }, InputFunction);
  var newtypeInputField = new Data_Newtype.Newtype(function (n) {
    return n;
  }, InputField);
  var newtypeFormField = new Data_Newtype.Newtype(function (n) {
    return n;
  }, FormField);

  var eqInputField = function eqInputField(dictEq) {
    return dictEq;
  };

  exports["FormProxy"] = FormProxy;
  exports["OutputField"] = OutputField;
  exports["FormField"] = FormField;
  exports["newtypeInputField"] = newtypeInputField;
  exports["eqInputField"] = eqInputField;
  exports["newtypeOutputField"] = newtypeOutputField;
  exports["newtypeInputFunction"] = newtypeInputFunction;
  exports["newtypeFormField"] = newtypeFormField;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Action"] = $PS["Formless.Action"] || {};
  var exports = $PS["Formless.Action"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Variant = $PS["Data.Variant"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var validateAll = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
    return "validateAll";
  }))(Data_Symbol.SProxy.value)(Data_Unit.unit);
  var submit = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
    return "submit";
  }))(Data_Symbol.SProxy.value)(Data_Unit.unit);

  var setValidate = function setValidate(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return function (i) {
            return Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
              return "modifyValidate";
            }))(Data_Symbol.SProxy.value)(new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Data_Newtype.wrap(dictNewtype)(Data_Variant.inj()(dictIsSymbol)(sym)(Data_Newtype.wrap(Formless_Types_Form.newtypeInputFunction)(Data_Function["const"](i))))));
          };
        };
      };
    };
  };

  var loadForm = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
    return "loadForm";
  }))(Data_Symbol.SProxy.value);
  var injAction = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
    return "userAction";
  }))(Data_Symbol.SProxy.value);
  exports["injAction"] = injAction;
  exports["setValidate"] = setValidate;
  exports["validateAll"] = validateAll;
  exports["submit"] = submit;
  exports["loadForm"] = loadForm;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Class.Initial"] = $PS["Formless.Class.Initial"] || {};
  var exports = $PS["Formless.Class.Initial"];
  var Data_Monoid = $PS["Data.Monoid"];

  var Initial = function Initial(initial) {
    this.initial = initial;
  };

  var initialString = new Initial(Data_Monoid.mempty(Data_Monoid.monoidString));

  var initial = function initial(dict) {
    return dict.initial;
  };

  exports["initial"] = initial;
  exports["initialString"] = initialString;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Data.FormFieldResult"] = $PS["Formless.Data.FormFieldResult"] || {};
  var exports = $PS["Formless.Data.FormFieldResult"];
  var Data_Either = $PS["Data.Either"];
  var Data_Lens_Prism = $PS["Data.Lens.Prism"];
  var Data_Maybe = $PS["Data.Maybe"];

  var NotValidated = function () {
    function NotValidated() {}

    ;
    NotValidated.value = new NotValidated();
    return NotValidated;
  }();

  var Validating = function () {
    function Validating() {}

    ;
    Validating.value = new Validating();
    return Validating;
  }();

  var $$Error = function () {
    function $$Error(value0) {
      this.value0 = value0;
    }

    ;

    $$Error.create = function (value0) {
      return new $$Error(value0);
    };

    return $$Error;
  }();

  var Success = function () {
    function Success(value0) {
      this.value0 = value0;
    }

    ;

    Success.create = function (value0) {
      return new Success(value0);
    };

    return Success;
  }();

  var toMaybe = function toMaybe(v) {
    if (v instanceof Success) {
      return new Data_Maybe.Just(v.value0);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var fromEither = function fromEither(v) {
    if (v instanceof Data_Either.Left) {
      return new $$Error(v.value0);
    }

    ;

    if (v instanceof Data_Either.Right) {
      return new Success(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Formless.Data.FormFieldResult (line 44, column 14 - line 46, column 23): " + [v.constructor.name]);
  };

  var _Error = function _Error(dictChoice) {
    return Data_Lens_Prism["prism'"]($$Error.create)(function (v) {
      if (v instanceof $$Error) {
        return new Data_Maybe.Just(v.value0);
      }

      ;
      return Data_Maybe.Nothing.value;
    })(dictChoice);
  };

  exports["NotValidated"] = NotValidated;
  exports["Validating"] = Validating;
  exports["Error"] = $$Error;
  exports["fromEither"] = fromEither;
  exports["toMaybe"] = toMaybe;
  exports["_Error"] = _Error;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Validation"] = $PS["Formless.Validation"] || {};
  var exports = $PS["Formless.Validation"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Validation = function Validation(x) {
    return x;
  };

  var newtypeValidation = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Validation);

  var runValidation = function runValidation(dictMonad) {
    return Data_Newtype.unwrap(newtypeValidation);
  };

  var semigroupoidValidation = function semigroupoidValidation(dictMonad) {
    return new Control_Semigroupoid.Semigroupoid(function (v1) {
      return function (v0) {
        return function (form) {
          return function (i) {
            return Control_Bind.bind(dictMonad.Bind1())(Data_Newtype.unwrap(newtypeValidation)(v0)(form)(i))(function (v) {
              return Data_Either.either(function () {
                var $52 = Control_Applicative.pure(dictMonad.Applicative0());
                return function ($53) {
                  return $52(Data_Either.Left.create($53));
                };
              }())(Data_Newtype.unwrap(newtypeValidation)(v1)(form))(v);
            });
          };
        };
      };
    });
  };

  var hoistFn_ = function hoistFn_(dictMonad) {
    return function (f) {
      return Validation(Data_Function["const"](function () {
        var $54 = Control_Applicative.pure(dictMonad.Applicative0());
        var $55 = Control_Applicative.pure(Data_Either.applicativeEither);
        return function ($56) {
          return $54($55(f($56)));
        };
      }()));
    };
  };

  var hoistFnE_ = function hoistFnE_(dictMonad) {
    return function (f) {
      return Validation(Data_Function["const"](function () {
        var $58 = Control_Applicative.pure(dictMonad.Applicative0());
        return function ($59) {
          return $58(f($59));
        };
      }()));
    };
  };

  exports["runValidation"] = runValidation;
  exports["hoistFn_"] = hoistFn_;
  exports["hoistFnE_"] = hoistFnE_;
  exports["newtypeValidation"] = newtypeValidation;
  exports["semigroupoidValidation"] = semigroupoidValidation;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Internal.Transform"] = $PS["Formless.Internal.Transform"] || {};
  var exports = $PS["Formless.Internal.Transform"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Formless_Data_FormFieldResult = $PS["Formless.Data.FormFieldResult"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var Formless_Validation = $PS["Formless.Validation"];
  var Record = $PS["Record"];
  var Record_Builder = $PS["Record.Builder"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var ValidateAll = function ValidateAll(validateAllBuilder) {
    this.validateAllBuilder = validateAllBuilder;
  };

  var SetFormFieldsTouched = function SetFormFieldsTouched(setFormFieldsTouchedBuilder) {
    this.setFormFieldsTouchedBuilder = setFormFieldsTouchedBuilder;
  };

  var ReplaceFormFieldInputs = function ReplaceFormFieldInputs(replaceFormFieldInputsBuilder) {
    this.replaceFormFieldInputsBuilder = replaceFormFieldInputsBuilder;
  };

  var ModifyAll = function ModifyAll(modifyAllBuilder) {
    this.modifyAllBuilder = modifyAllBuilder;
  };

  var InputFieldsToFormFields = function InputFieldsToFormFields(inputFieldsToFormFieldsBuilder) {
    this.inputFieldsToFormFieldsBuilder = inputFieldsToFormFieldsBuilder;
  };

  var FormFieldsToInputFields = function FormFieldsToInputFields(formFieldsToInputFieldsBuilder) {
    this.formFieldsToInputFieldsBuilder = formFieldsToInputFieldsBuilder;
  };

  var FormFieldToMaybeOutput = function FormFieldToMaybeOutput(formFieldsToMaybeOutputBuilder) {
    this.formFieldsToMaybeOutputBuilder = formFieldsToMaybeOutputBuilder;
  };

  var CountErrors = function CountErrors(countErrorsImpl) {
    this.countErrorsImpl = countErrorsImpl;
  };

  var AllTouched = function AllTouched(allTouchedImpl) {
    this.allTouchedImpl = allTouchedImpl;
  };

  var validateAllBuilder = function validateAllBuilder(dict) {
    return dict.validateAllBuilder;
  };

  var unsafeRunValidationVariant = function unsafeRunValidationVariant(dictMonad) {
    return function (dictNewtype) {
      return function (dictNewtype1) {
        return function (dictNewtype2) {
          return function ($$var) {
            return function (vs) {
              return function (rec) {
                var label = function () {
                  var v = Data_Newtype.unwrap(dictNewtype)($$var);
                  return v.type;
                }();

                var rec2 = function () {
                  var v = Record_Unsafe.unsafeGet(label)(Data_Newtype.unwrap(dictNewtype1)(rec));
                  return Control_Bind.bind(dictMonad.Bind1())(Formless_Validation.runValidation(dictMonad)(Record_Unsafe.unsafeGet(label)(Data_Newtype.unwrap(dictNewtype2)(vs)))(rec)(v.input))(function (v1) {
                    var rec$prime = Record_Unsafe.unsafeSet(label)(Formless_Types_Form.FormField({
                      input: v.input,
                      touched: v.touched,
                      result: Formless_Data_FormFieldResult.fromEither(v1)
                    }))(Data_Newtype.unwrap(dictNewtype1)(rec));
                    return Control_Applicative.pure(dictMonad.Applicative0())(Data_Newtype.wrap(dictNewtype1)(rec$prime));
                  });
                }();

                return rec2;
              };
            };
          };
        };
      };
    };
  };

  var unsafeModifyInputVariant = function unsafeModifyInputVariant(dictNewtype) {
    return function (dictNewtype1) {
      return function (f) {
        return function ($$var) {
          return function (rec) {
            var rep = function () {
              var v = Data_Newtype.unwrap(dictNewtype)($$var);
              return new Data_Tuple.Tuple(v.type, v.value);
            }();

            var val = function () {
              var v = Record_Unsafe.unsafeGet(Data_Tuple.fst(rep))(Data_Newtype.unwrap(dictNewtype1)(rec));
              return Formless_Types_Form.FormField({
                input: Data_Newtype.unwrap(Formless_Types_Form.newtypeInputFunction)(Data_Tuple.snd(rep))(v.input),
                touched: true,
                result: f(v.result)
              });
            }();

            return Data_Newtype.wrap(dictNewtype1)(Record_Unsafe.unsafeSet(Data_Tuple.fst(rep))(val)(Data_Newtype.unwrap(dictNewtype1)(rec)));
          };
        };
      };
    };
  };

  var setFormFieldsTouchedNil = new SetFormFieldsTouched(function (v) {
    return function (v1) {
      return Control_Category.identity(Record_Builder.categoryBuilder);
    };
  });

  var setFormFieldsTouchedBuilder = function setFormFieldsTouchedBuilder(dict) {
    return dict.setFormFieldsTouchedBuilder;
  };

  var setFormFieldsTouchedCons = function setFormFieldsTouchedCons(dictIsSymbol) {
    return function (dictCons) {
      return function (dictSetFormFieldsTouched) {
        return function (dictRow1Cons) {
          return new SetFormFieldsTouched(function (v) {
            return function (r) {
              var rest = setFormFieldsTouchedBuilder(dictSetFormFieldsTouched)(Type_Data_RowList.RLProxy.value)(r);
              var val = Data_Newtype.over(Formless_Types_Form.newtypeFormField)(Formless_Types_Form.newtypeFormField)(Formless_Types_Form.FormField)(function (v1) {
                return {
                  touched: true,
                  input: v1.input,
                  result: v1.result
                };
              })(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r));
              var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(val);
              return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
            };
          });
        };
      };
    };
  };

  var replaceFormFieldInputsTouchedNil = new ReplaceFormFieldInputs(function (v) {
    return function (v1) {
      return function (v2) {
        return Control_Category.identity(Record_Builder.categoryBuilder);
      };
    };
  });

  var replaceFormFieldInputsBuilder = function replaceFormFieldInputsBuilder(dict) {
    return dict.replaceFormFieldInputsBuilder;
  };

  var replaceFormFieldInputsTouchedCons = function replaceFormFieldInputsTouchedCons(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictNewtype1) {
        return function (dictCons) {
          return function (dictCons1) {
            return function (dictRow1Cons) {
              return function (dictReplaceFormFieldInputs) {
                return new ReplaceFormFieldInputs(function (ir) {
                  return function (v) {
                    return function (fr) {
                      var rest = replaceFormFieldInputsBuilder(dictReplaceFormFieldInputs)(ir)(Type_Data_RowList.RLProxy.value)(fr);
                      var f = Data_Newtype.unwrap(dictNewtype1)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(fr));
                      var i = Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(ir);
                      var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(Formless_Types_Form.FormField({
                        input: Data_Newtype.unwrap(dictNewtype)(i),
                        touched: false,
                        result: Formless_Data_FormFieldResult.NotValidated.value
                      }));
                      return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
                    };
                  };
                });
              };
            };
          };
        };
      };
    };
  };

  var nilCountErrors = new CountErrors(function (v) {
    return function (v1) {
      return 0;
    };
  });
  var nilAllTouched = new AllTouched(function (v) {
    return function (v1) {
      return true;
    };
  });
  var modifyAllNil = new ModifyAll(function (v) {
    return function (v1) {
      return function (v2) {
        return Control_Category.identity(Record_Builder.categoryBuilder);
      };
    };
  });

  var modifyAllBuilder = function modifyAllBuilder(dict) {
    return dict.modifyAllBuilder;
  };

  var modifyAllCons = function modifyAllCons(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictNewtype1) {
        return function (dictCons) {
          return function (dictCons1) {
            return function (dictRow1Cons) {
              return function (dictModifyAll) {
                return new ModifyAll(function (ifs) {
                  return function (v) {
                    return function (r) {
                      var rest = modifyAllBuilder(dictModifyAll)(ifs)(Type_Data_RowList.RLProxy.value)(r);
                      var f = Data_Newtype.unwrap(dictNewtype)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(ifs));
                      var field = Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r);
                      var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(Data_Newtype.over(dictNewtype1)(dictNewtype1)(Formless_Types_Form.FormField)(function (x) {
                        return {
                          input: f(x.input),
                          result: x.result,
                          touched: x.touched
                        };
                      })(field));
                      return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
                    };
                  };
                });
              };
            };
          };
        };
      };
    };
  };

  var inputFieldsToInputNil = new FormFieldsToInputFields(function (v) {
    return function (v1) {
      return Control_Category.identity(Record_Builder.categoryBuilder);
    };
  });
  var inputFieldsToFormFieldsNil = new InputFieldsToFormFields(function (v) {
    return function (v1) {
      return Control_Category.identity(Record_Builder.categoryBuilder);
    };
  });

  var inputFieldsToFormFieldsBuilder = function inputFieldsToFormFieldsBuilder(dict) {
    return dict.inputFieldsToFormFieldsBuilder;
  };

  var inputFieldsToFormFieldsCons = function inputFieldsToFormFieldsCons(dictIsSymbol) {
    return function (dictCons) {
      return function (dictInputFieldsToFormFields) {
        return function (dictRow1Cons) {
          return new InputFieldsToFormFields(function (v) {
            return function (r) {
              var transform = function transform(v1) {
                return {
                  input: v1,
                  touched: false,
                  result: Formless_Data_FormFieldResult.NotValidated.value
                };
              };

              var rest = inputFieldsToFormFieldsBuilder(dictInputFieldsToFormFields)(Type_Data_RowList.RLProxy.value)(r);
              var val = transform(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r));
              var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(val);
              return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
            };
          });
        };
      };
    };
  };

  var fromScratch = Data_Functor.flap(Data_Functor.functorFn)(Record_Builder.build)({});

  var inputFieldsToFormFields = function inputFieldsToFormFields(dictRowToList) {
    return function (dictInputFieldsToFormFields) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (r) {
            var builder = inputFieldsToFormFieldsBuilder(dictInputFieldsToFormFields)(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype)(r));
            return Data_Newtype.wrap(dictNewtype1)(fromScratch(builder));
          };
        };
      };
    };
  };

  var modifyAll = function modifyAll(dictRowToList) {
    return function (dictModifyAll) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (ifs) {
            return function (fs) {
              var builder = modifyAllBuilder(dictModifyAll)(Data_Newtype.unwrap(dictNewtype)(ifs))(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype1)(fs));
              return Data_Newtype.wrap(dictNewtype1)(fromScratch(builder));
            };
          };
        };
      };
    };
  };

  var replaceFormFieldInputs = function replaceFormFieldInputs(dictRowToList) {
    return function (dictReplaceFormFieldInputs) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (is) {
            return function (fs) {
              var builder = replaceFormFieldInputsBuilder(dictReplaceFormFieldInputs)(Data_Newtype.unwrap(dictNewtype)(is))(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype1)(fs));
              return Data_Newtype.wrap(dictNewtype1)(fromScratch(builder));
            };
          };
        };
      };
    };
  };

  var setFormFieldsTouched = function setFormFieldsTouched(dictRowToList) {
    return function (dictSetFormFieldsTouched) {
      return function (dictNewtype) {
        return function (r) {
          var builder = setFormFieldsTouchedBuilder(dictSetFormFieldsTouched)(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype)(r));
          return Data_Newtype.wrap(dictNewtype)(fromScratch(builder));
        };
      };
    };
  };

  var validateAll = function validateAll(dictRowToList) {
    return function (dictMonad) {
      return function (dictValidateAll) {
        return function (dictNewtype) {
          return function (dictNewtype1) {
            return function (vs) {
              return function (fs) {
                var builder = validateAllBuilder(dictValidateAll)(Data_Newtype.unwrap(dictNewtype)(vs))(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype1)(fs));
                return Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(Data_Newtype.wrap(dictNewtype1))(Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(fromScratch)(builder));
              };
            };
          };
        };
      };
    };
  };

  var formFieldsToMaybeOutputNil = new FormFieldToMaybeOutput(function (v) {
    return function (v1) {
      return new Data_Maybe.Just(Control_Category.identity(Record_Builder.categoryBuilder));
    };
  });

  var formFieldsToMaybeOutputBuilder = function formFieldsToMaybeOutputBuilder(dict) {
    return dict.formFieldsToMaybeOutputBuilder;
  };

  var formFieldsToMaybeOutputCons = function formFieldsToMaybeOutputCons(dictIsSymbol) {
    return function (dictCons) {
      return function (dictFormFieldToMaybeOutput) {
        return function (dictRow1Cons) {
          return new FormFieldToMaybeOutput(function (v) {
            return function (r) {
              var rest = formFieldsToMaybeOutputBuilder(dictFormFieldToMaybeOutput)(Type_Data_RowList.RLProxy.value)(r);

              var transform = function transform(v1) {
                return function (builder$prime) {
                  return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(v1))(builder$prime);
                };
              };

              var val = Data_Functor.map(Data_Maybe.functorMaybe)(Formless_Types_Form.OutputField)(Formless_Data_FormFieldResult.toMaybe(Data_Newtype.unwrap(Formless_Types_Form.newtypeFormField)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r)).result));
              return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(transform)(val))(rest);
            };
          });
        };
      };
    };
  };

  var formFieldsToMaybeOutputFields = function formFieldsToMaybeOutputFields(dictRowToList) {
    return function (dictNewtype) {
      return function (dictNewtype1) {
        return function (dictFormFieldToMaybeOutput) {
          return function (r) {
            var builder = formFieldsToMaybeOutputBuilder(dictFormFieldToMaybeOutput)(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype)(r));
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap(dictNewtype1))(Data_Functor.map(Data_Maybe.functorMaybe)(fromScratch)(builder));
          };
        };
      };
    };
  };

  var formFieldsToInputFieldsBuilder = function formFieldsToInputFieldsBuilder(dict) {
    return dict.formFieldsToInputFieldsBuilder;
  };

  var inputFieldsToInputCons = function inputFieldsToInputCons(dictIsSymbol) {
    return function (dictCons) {
      return function (dictFormFieldsToInputFields) {
        return function (dictRow1Cons) {
          return new FormFieldsToInputFields(function (v) {
            return function (r) {
              var transform = function transform(v1) {
                return v1.input;
              };

              var rest = formFieldsToInputFieldsBuilder(dictFormFieldsToInputFields)(Type_Data_RowList.RLProxy.value)(r);
              var val = transform(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r));
              var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(val);
              return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
            };
          });
        };
      };
    };
  };

  var formFieldsToInputFields = function formFieldsToInputFields(dictRowToList) {
    return function (dictFormFieldsToInputFields) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (r) {
            var builder = formFieldsToInputFieldsBuilder(dictFormFieldsToInputFields)(Type_Data_RowList.RLProxy.value)(Data_Newtype.unwrap(dictNewtype1)(r));
            return Data_Newtype.wrap(dictNewtype)(fromScratch(builder));
          };
        };
      };
    };
  };

  var countErrorsImpl = function countErrorsImpl(dict) {
    return dict.countErrorsImpl;
  };

  var countErrors = function countErrors(dictRowToList) {
    return function (dictCountErrors) {
      return function (dictNewtype) {
        var $146 = countErrorsImpl(dictCountErrors)(Type_Data_RowList.RLProxy.value);
        var $147 = Data_Newtype.unwrap(dictNewtype);
        return function ($148) {
          return $146($147($148));
        };
      };
    };
  };

  var consCountErrors = function consCountErrors(dictIsSymbol) {
    return function (dictCons) {
      return function (dictCountErrors) {
        return new CountErrors(function (v) {
          return function (r) {
            var res = function () {
              var v1 = Data_Newtype.unwrap(Formless_Types_Form.newtypeFormField)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r)).result;

              if (v1 instanceof Formless_Data_FormFieldResult["Error"]) {
                return 1;
              }

              ;
              return 0;
            }();

            return res + countErrorsImpl(dictCountErrors)(Type_Data_RowList.RLProxy.value)(r) | 0;
          };
        });
      };
    };
  };

  var applyToValidationNil = function applyToValidationNil(dictMonad) {
    return new ValidateAll(function (v) {
      return function (v1) {
        return function (v2) {
          return Control_Applicative.pure(dictMonad.Applicative0())(Control_Category.identity(Record_Builder.categoryBuilder));
        };
      };
    });
  };

  var applyToValidationCons = function applyToValidationCons(dictIsSymbol) {
    return function (dictMonad) {
      return function (dictCons) {
        return function (dictNewtype) {
          return function (dictCons1) {
            return function (dictRow1Cons) {
              return function (dictValidateAll) {
                return new ValidateAll(function (vs) {
                  return function (v) {
                    return function (r) {
                      var rest = validateAllBuilder(dictValidateAll)(vs)(Type_Data_RowList.RLProxy.value)(r);

                      var fn = function fn(val$prime) {
                        return function (rest$prime) {
                          return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(val$prime))(rest$prime);
                        };
                      };

                      var val = function () {
                        var validator = Data_Newtype.unwrap(Formless_Validation.newtypeValidation)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(vs));
                        var formField = Data_Newtype.unwrap(Formless_Types_Form.newtypeFormField)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r));
                        return Control_Bind.bind(dictMonad.Bind1())(validator(Data_Newtype.wrap(dictNewtype)(r))(formField.input))(function (v1) {
                          return Control_Applicative.pure(dictMonad.Applicative0())(Data_Newtype.wrap(Formless_Types_Form.newtypeFormField)(function () {
                            var $142 = {};

                            for (var $143 in formField) {
                              if ({}.hasOwnProperty.call(formField, $143)) {
                                $142[$143] = formField[$143];
                              }

                              ;
                            }

                            ;
                            $142.result = Formless_Data_FormFieldResult.fromEither(v1);
                            return $142;
                          }()));
                        });
                      }();

                      return Control_Apply.apply(dictMonad.Bind1().Apply0())(Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(fn)(val))(rest);
                    };
                  };
                });
              };
            };
          };
        };
      };
    };
  };

  var allTouchedImpl = function allTouchedImpl(dict) {
    return dict.allTouchedImpl;
  };

  var consAllTouched = function consAllTouched(dictIsSymbol) {
    return function (dictCons) {
      return function (dictAllTouched) {
        return new AllTouched(function (v) {
          return function (r) {
            var $145 = Data_Newtype.unwrap(Formless_Types_Form.newtypeFormField)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r)).touched;

            if ($145) {
              return allTouchedImpl(dictAllTouched)(Type_Data_RowList.RLProxy.value)(r);
            }

            ;
            return false;
          };
        });
      };
    };
  };

  var allTouched = function allTouched(dictRowToList) {
    return function (dictAllTouched) {
      return function (dictNewtype) {
        var $149 = allTouchedImpl(dictAllTouched)(Type_Data_RowList.RLProxy.value);
        var $150 = Data_Newtype.unwrap(dictNewtype);
        return function ($151) {
          return $149($150($151));
        };
      };
    };
  };

  exports["fromScratch"] = fromScratch;
  exports["allTouched"] = allTouched;
  exports["countErrors"] = countErrors;
  exports["setFormFieldsTouched"] = setFormFieldsTouched;
  exports["formFieldsToInputFields"] = formFieldsToInputFields;
  exports["inputFieldsToFormFields"] = inputFieldsToFormFields;
  exports["formFieldsToMaybeOutputFields"] = formFieldsToMaybeOutputFields;
  exports["replaceFormFieldInputs"] = replaceFormFieldInputs;
  exports["modifyAll"] = modifyAll;
  exports["validateAll"] = validateAll;
  exports["unsafeModifyInputVariant"] = unsafeModifyInputVariant;
  exports["unsafeRunValidationVariant"] = unsafeRunValidationVariant;
  exports["setFormFieldsTouchedNil"] = setFormFieldsTouchedNil;
  exports["setFormFieldsTouchedCons"] = setFormFieldsTouchedCons;
  exports["inputFieldsToInputNil"] = inputFieldsToInputNil;
  exports["inputFieldsToInputCons"] = inputFieldsToInputCons;
  exports["inputFieldsToFormFieldsNil"] = inputFieldsToFormFieldsNil;
  exports["inputFieldsToFormFieldsCons"] = inputFieldsToFormFieldsCons;
  exports["formFieldsToMaybeOutputNil"] = formFieldsToMaybeOutputNil;
  exports["formFieldsToMaybeOutputCons"] = formFieldsToMaybeOutputCons;
  exports["nilCountErrors"] = nilCountErrors;
  exports["consCountErrors"] = consCountErrors;
  exports["nilAllTouched"] = nilAllTouched;
  exports["consAllTouched"] = consAllTouched;
  exports["applyToValidationNil"] = applyToValidationNil;
  exports["applyToValidationCons"] = applyToValidationCons;
  exports["modifyAllNil"] = modifyAllNil;
  exports["modifyAllCons"] = modifyAllCons;
  exports["replaceFormFieldInputsTouchedNil"] = replaceFormFieldInputsTouchedNil;
  exports["replaceFormFieldInputsTouchedCons"] = replaceFormFieldInputsTouchedCons;
})(PS);

(function ($PS) {
  "use strict";

  $PS["Formless.Types.Component"] = $PS["Formless.Types.Component"] || {};
  var exports = $PS["Formless.Types.Component"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Halogen_Query_ChildQuery = $PS["Halogen.Query.ChildQuery"]; // | A type to represent validation status

  var Invalid = function () {
    function Invalid() {}

    ;
    Invalid.value = new Invalid();
    return Invalid;
  }(); // | A type to represent validation status


  var Incomplete = function () {
    function Incomplete() {}

    ;
    Incomplete.value = new Incomplete();
    return Incomplete;
  }(); // | A type to represent validation status


  var Valid = function () {
    function Valid() {}

    ;
    Valid.value = new Valid();
    return Valid;
  }(); // | The internals of the public component query type. Many of these are shared
  // | with actions of the same name so they can be used in rendering. See
  // | `Formless.Action` and `Formless.Query` for more.


  var SubmitReply = function () {
    function SubmitReply(value0) {
      this.value0 = value0;
    }

    ;

    SubmitReply.create = function (value0) {
      return new SubmitReply(value0);
    };

    return SubmitReply;
  }(); // | The internals of the public component query type. Many of these are shared
  // | with actions of the same name so they can be used in rendering. See
  // | `Formless.Action` and `Formless.Query` for more.


  var SendQuery = function () {
    function SendQuery(value0) {
      this.value0 = value0;
    }

    ;

    SendQuery.create = function (value0) {
      return new SendQuery(value0);
    };

    return SendQuery;
  }(); // | The internals of the public component query type. Many of these are shared
  // | with actions of the same name so they can be used in rendering. See
  // | `Formless.Action` and `Formless.Query` for more.


  var AsQuery = function () {
    function AsQuery(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    AsQuery.create = function (value0) {
      return function (value1) {
        return new AsQuery(value0, value1);
      };
    };

    return AsQuery;
  }(); // | The component tries to require as few messages to be handled as possible. You
  // | can always use the *Reply variants of queries to perform actions and receive
  // | a result out the other end, or extend these messages.


  var Submitted = function () {
    function Submitted(value0) {
      this.value0 = value0;
    }

    ;

    Submitted.create = function (value0) {
      return new Submitted(value0);
    };

    return Submitted;
  }(); // | The component tries to require as few messages to be handled as possible. You
  // | can always use the *Reply variants of queries to perform actions and receive
  // | a result out the other end, or extend these messages.


  var Changed = function () {
    function Changed(value0) {
      this.value0 = value0;
    }

    ;

    Changed.create = function (value0) {
      return new Changed(value0);
    };

    return Changed;
  }(); // | A newtype to make easier type errors for end users to
  // | read by hiding internal fields


  var InternalState = function InternalState(x) {
    return x;
  };

  var newtypeInternalState = new Data_Newtype.Newtype(function (n) {
    return n;
  }, InternalState);
  var functorQueryF = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof SubmitReply) {
        return new SubmitReply(Data_Functor.map(Data_Functor.functorFn)(f)(m.value0));
      }

      ;

      if (m instanceof SendQuery) {
        return new SendQuery(Data_Functor.map(Halogen_Query_ChildQuery.functorChildQuery)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(m.value0));
      }

      ;

      if (m instanceof AsQuery) {
        return new AsQuery(m.value0, f(m.value1));
      }

      ;
      throw new Error("Failed pattern match at Formless.Types.Component (line 88, column 1 - line 88, column 61): " + [m.constructor.name]);
    };
  }); // | A convenience export of formless as a symbol for use when mounting Formless
  // | as a child component
  // |
  // | ```purescript
  // | type ChildSlots = (formless :: F.Slot' Form FormResult)
  // | HH.slot F._formless unit (F.component spec) input handler
  // | ```

  var _formless = Data_Symbol.SProxy.value;
  exports["SubmitReply"] = SubmitReply;
  exports["SendQuery"] = SendQuery;
  exports["AsQuery"] = AsQuery;
  exports["InternalState"] = InternalState;
  exports["Invalid"] = Invalid;
  exports["Incomplete"] = Incomplete;
  exports["Valid"] = Valid;
  exports["Submitted"] = Submitted;
  exports["Changed"] = Changed;
  exports["_formless"] = _formless;
  exports["functorQueryF"] = functorQueryF;
  exports["newtypeInternalState"] = newtypeInternalState;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Internal.Component"] = $PS["Formless.Internal.Component"] || {};
  var exports = $PS["Formless.Internal.Component"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Record_Builder = $PS["Record.Builder"];

  var submit = function submit(dictMonadAff) {
    return function (dictRowToList) {
      return function (dictAllTouched) {
        return function (dictSetFormFieldsTouched) {
          return function (dictValidateAll) {
            return function (dictFormFieldToMaybeOutput) {
              return function (dictValidateAll1) {
                return function (dictNewtype) {
                  return function (dictNewtype1) {
                    return function (dictNewtype2) {
                      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                          var $28 = {};

                          for (var $29 in v1) {
                            if ({}.hasOwnProperty.call(v1, $29)) {
                              $28[$29] = v1[$29];
                            }

                            ;
                          }

                          ;
                          $28.submitting = false;
                          return $28;
                        }))(function () {
                          return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(function () {
                            if (v.validity instanceof Formless_Types_Component.Valid) {
                              return Formless_Internal_Transform.formFieldsToMaybeOutputFields()(dictNewtype)(dictNewtype1)(dictFormFieldToMaybeOutput)(v.form);
                            }

                            ;
                            return Data_Maybe.Nothing.value;
                          }());
                        });
                      });
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  var preSubmit = function preSubmit(dictMonadAff) {
    return function (dictRowToList) {
      return function (dictAllTouched) {
        return function (dictSetFormFieldsTouched) {
          return function (dictValidateAll) {
            return function (dictFormFieldToMaybeOutput) {
              return function (dictValidateAll1) {
                return function (dictNewtype) {
                  return function (dictNewtype1) {
                    return function (dictNewtype2) {
                      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                        var $32 = {};

                        for (var $33 in st) {
                          if ({}.hasOwnProperty.call(st, $33)) {
                            $32[$33] = st[$33];
                          }

                          ;
                        }

                        ;
                        $32.submitAttempts = st.submitAttempts + 1 | 0;
                        $32.submitting = true;
                        return $32;
                      }))(function (v) {
                        var internal = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v.internal);
                        return Control_Applicative.when(Halogen_Query_HalogenM.applicativeHalogenM)(!internal.allTouched)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                          var $36 = {};

                          for (var $37 in v1) {
                            if ({}.hasOwnProperty.call(v1, $37)) {
                              $36[$37] = v1[$37];
                            }

                            ;
                          }

                          ;
                          $36.form = Formless_Internal_Transform.setFormFieldsTouched()(dictSetFormFieldsTouched)(dictNewtype)(v.form);
                          $36.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(function (v2) {
                            return {
                              allTouched: true,
                              debounceRef: v2.debounceRef,
                              initialInputs: v2.initialInputs,
                              validationRef: v2.validationRef,
                              validators: v2.validators
                            };
                          })(v.internal);
                          return $36;
                        }));
                      });
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  var getPublicState = function getPublicState(dictLacks) {
    return Record_Builder.build(Record_Builder["delete"](new Data_Symbol.IsSymbol(function () {
      return "internal";
    }))()()(Data_Symbol.SProxy.value));
  };

  exports["getPublicState"] = getPublicState;
  exports["preSubmit"] = preSubmit;
  exports["submit"] = submit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Internal.Debounce"] = $PS["Formless.Internal.Debounce"] || {};
  var exports = $PS["Formless.Internal.Debounce"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_AVar = $PS["Effect.Aff.AVar"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];

  var debounceForm = function debounceForm(dictMonadAff) {
    return function (ms) {
      return function (pre) {
        return function (post) {
          return function (last) {
            var readRef = function readRef(dictMonadAff1) {
              var $29 = Effect_Class.liftEffect(dictMonadAff1.MonadEffect0());
              var $30 = Data_Functor.map(Effect.functorEffect)(Control_Bind.join(Data_Maybe.bindMaybe));
              var $31 = Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(Effect_Ref.read);
              return function ($32) {
                return $29($30($31($32)));
              };
            };

            var mkFiber = function mkFiber(v) {
              return Effect_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(Effect_Aff.forkAff(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff.delay(ms))(function () {
                return Effect_Aff_AVar.put(Data_Unit.unit)(v);
              })));
            };

            var killFiber$prime = function killFiber$prime(dictMonadAff1) {
              var $33 = Effect_Aff_Class.liftAff(dictMonadAff1);
              var $34 = Effect_Aff.killFiber(Effect_Exception.error("time's up!"));
              return function ($35) {
                return $33($34($35));
              };
            };

            var atomic = function atomic(dictMonadAff1) {
              return function (process) {
                return function (maybeLast) {
                  return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
                    var ref = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v.internal).validationRef;
                    return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(readRef(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff1))(ref))(function (v1) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Data_Foldable.for_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(v1)(Halogen_Query_HalogenM.kill))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff1.MonadEffect0()))(Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(ref)(Effect_Ref.write(Data_Maybe.Nothing.value))))(function () {
                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.fork(Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(process)(function (v2) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                              var $18 = {};

                              for (var $19 in v3) {
                                if ({}.hasOwnProperty.call(v3, $19)) {
                                  $18[$19] = v3[$19];
                                }

                                ;
                              }

                              ;
                              $18.form = v2;
                              return $18;
                            }))(function () {
                              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff1.MonadEffect0()))(Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(ref)(Effect_Ref.write(Data_Maybe.Nothing.value))))(function () {
                                return Data_Foldable.for_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(maybeLast)(Control_Category.identity(Control_Category.categoryFn));
                              });
                            });
                          })))(function (v2) {
                            return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff1.MonadEffect0()))(Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(ref)(Effect_Ref.write(new Data_Maybe.Just(v2))));
                          });
                        });
                      });
                    });
                  });
                };
              };
            };

            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
              var vdRef = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v.internal).validationRef;
              var dbRef = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v.internal).debounceRef;
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(readRef(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(vdRef))(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(Halogen_Query_HalogenM.kill)))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Data_Functor.map(Effect.functorEffect)(Control_Bind.join(Data_Maybe.bindMaybe))(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Effect.applicativeEffect)(Effect_Ref.read)(dbRef))))(function (v1) {
                  if (v1 instanceof Data_Maybe.Nothing) {
                    return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(Effect_Aff_AVar.empty))(function (v2) {
                      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(mkFiber(v2))(function (v3) {
                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.fork(Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Effect_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(Effect_Aff_AVar.take(v2))))(function () {
                          return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(Effect_Ref.write(Data_Maybe.Nothing.value))(dbRef)))(function () {
                            return atomic(dictMonadAff)(post)(new Data_Maybe.Just(last));
                          });
                        })))(function (v4) {
                          return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(dbRef)(Effect_Ref.write(new Data_Maybe.Just({
                            "var": v2,
                            fiber: v3
                          })))))(function () {
                            return atomic(dictMonadAff)(pre)(Data_Maybe.Nothing.value);
                          });
                        });
                      });
                    });
                  }

                  ;

                  if (v1 instanceof Data_Maybe.Just) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(killFiber$prime(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(v1.value0.fiber)))(function () {
                      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(mkFiber(v1["value0"]["var"]))(function (v2) {
                        return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(dbRef)(Effect_Ref.write(new Data_Maybe.Just({
                          "var": v1["value0"]["var"],
                          fiber: v2
                        }))));
                      });
                    });
                  }

                  ;
                  throw new Error("Failed pattern match at Formless.Internal.Debounce (line 40, column 3 - line 57, column 66): " + [v1.constructor.name]);
                });
              });
            });
          };
        };
      };
    };
  };

  exports["debounceForm"] = debounceForm;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Heterogeneous.Mapping"] = $PS["Heterogeneous.Mapping"] || {};
  var exports = $PS["Heterogeneous.Mapping"];
  var Control_Category = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Builder = $PS["Record.Builder"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var ConstMapping = function ConstMapping(x) {
    return x;
  };

  var MappingWithIndex = function MappingWithIndex(mappingWithIndex) {
    this.mappingWithIndex = mappingWithIndex;
  };

  var Mapping = function Mapping(mapping) {
    this.mapping = mapping;
  };

  var MapRecordWithIndex = function MapRecordWithIndex(mapRecordWithIndexBuilder) {
    this.mapRecordWithIndexBuilder = mapRecordWithIndexBuilder;
  };

  var HMap = function HMap(hmap) {
    this.hmap = hmap;
  };

  var mappingWithIndex = function mappingWithIndex(dict) {
    return dict.mappingWithIndex;
  };

  var mapping = function mapping(dict) {
    return dict.mapping;
  };

  var mapRecordWithIndexNil = new MapRecordWithIndex(function (v) {
    return function (v1) {
      return Control_Category.identity(Record_Builder.categoryBuilder);
    };
  });

  var mapRecordWithIndexBuilder = function mapRecordWithIndexBuilder(dict) {
    return dict.mapRecordWithIndexBuilder;
  };

  var mapRecordWithIndexCons = function mapRecordWithIndexCons(dictIsSymbol) {
    return function (dictMappingWithIndex) {
      return function (dictMapRecordWithIndex) {
        return function (dictCons) {
          return function (dictCons1) {
            return new MapRecordWithIndex(function (v) {
              return function (f) {
                return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(Record_Builder.modify()()(dictIsSymbol)(Data_Symbol.SProxy.value)(mappingWithIndex(dictMappingWithIndex)(f)(Data_Symbol.SProxy.value)))(mapRecordWithIndexBuilder(dictMapRecordWithIndex)(Type_Data_RowList.RLProxy.value)(f));
              };
            });
          };
        };
      };
    };
  };

  var hmapRecord = function hmapRecord(dictRowToList) {
    return function (dictMapRecordWithIndex) {
      return new HMap(function () {
        var $77 = mapRecordWithIndexBuilder(dictMapRecordWithIndex)(Type_Data_RowList.RLProxy.value);
        return function ($78) {
          return Record_Builder.build($77(ConstMapping($78)));
        };
      }());
    };
  };

  var hmap = function hmap(dict) {
    return dict.hmap;
  };

  var constMapping = function constMapping(dictMapping) {
    return new MappingWithIndex(function (v) {
      return function (v1) {
        return mapping(dictMapping)(v);
      };
    });
  };

  exports["hmap"] = hmap;
  exports["Mapping"] = Mapping;
  exports["constMapping"] = constMapping;
  exports["hmapRecord"] = hmapRecord;
  exports["mapRecordWithIndexCons"] = mapRecordWithIndexCons;
  exports["mapRecordWithIndexNil"] = mapRecordWithIndexNil;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Transform.Record"] = $PS["Formless.Transform.Record"] || {};
  var exports = $PS["Formless.Transform.Record"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Heterogeneous_Mapping = $PS["Heterogeneous.Mapping"];

  var WrapField = function () {
    function WrapField() {}

    ;
    WrapField.value = new WrapField();
    return WrapField;
  }();

  var UnwrapField = function () {
    function UnwrapField() {}

    ;
    UnwrapField.value = new UnwrapField();
    return UnwrapField;
  }();

  var wrapRecord = function wrapRecord(dictHMap) {
    return Heterogeneous_Mapping.hmap(dictHMap)(WrapField.value);
  };

  var wrapInputFields = function wrapInputFields(dictNewtype) {
    return function (dictHMap) {
      var $17 = Data_Newtype.wrap(dictNewtype);
      var $18 = wrapRecord(dictHMap);
      return function ($19) {
        return $17($18($19));
      };
    };
  };

  var wrapField = function wrapField(dictNewtype) {
    return new Heterogeneous_Mapping.Mapping(function (v) {
      return Data_Newtype.wrap(dictNewtype);
    });
  };

  var unwrapRecord = function unwrapRecord(dictHMap) {
    return Heterogeneous_Mapping.hmap(dictHMap)(UnwrapField.value);
  };

  var unwrapOutputFields = function unwrapOutputFields(dictNewtype) {
    return function (dictHMap) {
      var $20 = unwrapRecord(dictHMap);
      var $21 = Data_Newtype.unwrap(dictNewtype);
      return function ($22) {
        return $20($21($22));
      };
    };
  };

  var unwrapField = function unwrapField(dictNewtype) {
    return new Heterogeneous_Mapping.Mapping(function (v) {
      return Data_Newtype.unwrap(dictNewtype);
    });
  };

  exports["unwrapOutputFields"] = unwrapOutputFields;
  exports["wrapInputFields"] = wrapInputFields;
  exports["unwrapField"] = unwrapField;
  exports["wrapField"] = wrapField;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Type.Data.Row"] = $PS["Type.Data.Row"] || {};
  var exports = $PS["Type.Data.Row"];

  var RProxy = function () {
    function RProxy() {}

    ;
    RProxy.value = new RProxy();
    return RProxy;
  }();

  exports["RProxy"] = RProxy;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Transform.Row"] = $PS["Formless.Transform.Row"] || {};
  var exports = $PS["Formless.Transform.Row"];
  var Control_Category = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Formless_Class_Initial = $PS["Formless.Class.Initial"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Record_Builder = $PS["Record.Builder"];
  var Type_Data_Row = $PS["Type.Data.Row"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var MakeSProxies = function MakeSProxies(makeSProxiesBuilder) {
    this.makeSProxiesBuilder = makeSProxiesBuilder;
  };

  var MakeInputFieldsFromRow = function MakeInputFieldsFromRow(mkInputFieldsFromRowBuilder) {
    this.mkInputFieldsFromRowBuilder = mkInputFieldsFromRowBuilder;
  };

  var mkInputFieldsFromRowNil = new MakeInputFieldsFromRow(function (v) {
    return function (v1) {
      return Control_Category.identity(Record_Builder.categoryBuilder);
    };
  });

  var mkInputFieldsFromRowBuilder = function mkInputFieldsFromRowBuilder(dict) {
    return dict.mkInputFieldsFromRowBuilder;
  };

  var mkInputFieldsFromRowCons = function mkInputFieldsFromRowCons(dictIsSymbol) {
    return function (dictInitial) {
      return function (dictCons) {
        return function (dictMakeInputFieldsFromRow) {
          return function (dictRow1Cons) {
            return new MakeInputFieldsFromRow(function (v) {
              return function (r) {
                var val = Formless_Class_Initial.initial(dictInitial);
                var rest = mkInputFieldsFromRowBuilder(dictMakeInputFieldsFromRow)(Type_Data_RowList.RLProxy.value)(r);
                var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(val);
                return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
              };
            });
          };
        };
      };
    };
  };

  var mkInputFields = function mkInputFields(dictRowToList) {
    return function (dictNewtype) {
      return function (dictMakeInputFieldsFromRow) {
        return function (v) {
          var builder = mkInputFieldsFromRowBuilder(dictMakeInputFieldsFromRow)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value);
          return Data_Newtype.wrap(dictNewtype)(Formless_Internal_Transform.fromScratch(builder));
        };
      };
    };
  };

  var makeSProxiesNil = new MakeSProxies(function (v) {
    return Control_Category.identity(Record_Builder.categoryBuilder);
  });

  var makeSProxiesBuilder = function makeSProxiesBuilder(dict) {
    return dict.makeSProxiesBuilder;
  };

  var makeSProxiesCons = function makeSProxiesCons(dictIsSymbol) {
    return function (dictRow1Cons) {
      return function (dictMakeSProxies) {
        return new MakeSProxies(function (v) {
          var rest = makeSProxiesBuilder(dictMakeSProxies)(Type_Data_RowList.RLProxy.value);
          var first = Record_Builder.insert()()(dictIsSymbol)(Data_Symbol.SProxy.value)(Data_Symbol.SProxy.value);
          return Control_Semigroupoid.compose(Record_Builder.semigroupoidBuilder)(first)(rest);
        });
      };
    };
  };

  var mkSProxies = function mkSProxies(dictRowToList) {
    return function (dictNewtype) {
      return function (dictMakeSProxies) {
        return function (v) {
          var builder = makeSProxiesBuilder(dictMakeSProxies)(Type_Data_RowList.RLProxy.value);
          return Formless_Internal_Transform.fromScratch(builder);
        };
      };
    };
  };

  exports["mkInputFields"] = mkInputFields;
  exports["mkSProxies"] = mkSProxies;
  exports["mkInputFieldsFromRowNil"] = mkInputFieldsFromRowNil;
  exports["mkInputFieldsFromRowCons"] = mkInputFieldsFromRowCons;
  exports["makeSProxiesNil"] = makeSProxiesNil;
  exports["makeSProxiesCons"] = makeSProxiesCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Component"] = $PS["Formless.Component"] || {};
  var exports = $PS["Formless.Component"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Free = $PS["Control.Monad.Free"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Functor_Variant = $PS["Data.Functor.Variant"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Variant = $PS["Data.Variant"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Formless_Action = $PS["Formless.Action"];
  var Formless_Data_FormFieldResult = $PS["Formless.Data.FormFieldResult"];
  var Formless_Internal_Component = $PS["Formless.Internal.Component"];
  var Formless_Internal_Debounce = $PS["Formless.Internal.Debounce"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Formless_Transform_Record = $PS["Formless.Transform.Record"];
  var Formless_Transform_Row = $PS["Formless.Transform.Row"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Record_Builder = $PS["Record.Builder"];

  var raiseResult = function raiseResult(dictNewtype) {
    return function (dictHMap) {
      return function (v) {
        if (v instanceof Formless_Types_Component.Submitted) {
          return Halogen_Query_HalogenM.raise(Formless_Transform_Record.unwrapOutputFields(dictNewtype)(dictHMap)(v.value0));
        }

        ;
        return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
      };
    };
  };

  var _handleAction = function handleAction(dictMonadAff) {
    return function (dictRowToList) {
      return function (dictRowToList1) {
        return function (dictEqRecord) {
          return function (dictInputFieldsToFormFields) {
            return function (dictFormFieldsToInputFields) {
              return function (dictCountErrors) {
                return function (dictAllTouched) {
                  return function (dictSetFormFieldsTouched) {
                    return function (dictReplaceFormFieldInputs) {
                      return function (dictModifyAll) {
                        return function (dictValidateAll) {
                          return function (dictFormFieldToMaybeOutput) {
                            return function (dictNewtype) {
                              return function (dictNewtype1) {
                                return function (dictNewtype2) {
                                  return function (dictNewtype3) {
                                    return function (dictNewtype4) {
                                      return function (dictNewtype5) {
                                        return function (dictNewtype6) {
                                          return function (dictNewtype7) {
                                            return function (dictLacks) {
                                              return function (handleAction$prime) {
                                                return function (handleEvent) {
                                                  return function (action) {
                                                    var sync = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
                                                      return "syncFormData";
                                                    }))(Data_Symbol.SProxy.value)(Data_Unit.unit);
                                                    return Data_Function.flip(Data_Variant.match()()())(action)({
                                                      initialize: function initialize(mbAction) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Effect_Ref["new"](Data_Maybe.Nothing.value)))(function (v) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Effect_Ref["new"](Data_Maybe.Nothing.value)))(function (v1) {
                                                            var setFields = function setFields(rec) {
                                                              return {
                                                                debounceRef: new Data_Maybe.Just(v),
                                                                validationRef: new Data_Maybe.Just(v1),
                                                                allTouched: rec.allTouched,
                                                                initialInputs: rec.initialInputs,
                                                                validators: rec.validators
                                                              };
                                                            };

                                                            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                              var $132 = {};

                                                              for (var $133 in st) {
                                                                if ({}.hasOwnProperty.call(st, $133)) {
                                                                  $132[$133] = st[$133];
                                                                }

                                                                ;
                                                              }

                                                              ;
                                                              $132.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(setFields)(st.internal);
                                                              return $132;
                                                            }))(function () {
                                                              return Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(handleAction$prime)(mbAction);
                                                            });
                                                          });
                                                        });
                                                      },
                                                      syncFormData: function syncFormData(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                                                          var errors = Formless_Internal_Transform.countErrors()(dictCountErrors)(dictNewtype2)(v1.form);
                                                          var dirty = !Data_Eq.eq(Data_Eq.eqRec()(dictEqRecord))(Data_Newtype.unwrap(dictNewtype)(Formless_Internal_Transform.formFieldsToInputFields()(dictFormFieldsToInputFields)(dictNewtype)(dictNewtype2)(v1.form)))(Data_Newtype.unwrap(dictNewtype)(Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v1.internal).initialInputs));
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(function () {
                                                            var v2 = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v1.internal).allTouched;

                                                            if (v2) {
                                                              return Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                                                                var $138 = {};

                                                                for (var $139 in v3) {
                                                                  if ({}.hasOwnProperty.call(v3, $139)) {
                                                                    $138[$139] = v3[$139];
                                                                  }

                                                                  ;
                                                                }

                                                                ;

                                                                $138.validity = function () {
                                                                  var $137 = errors === 0;

                                                                  if ($137) {
                                                                    return Formless_Types_Component.Valid.value;
                                                                  }

                                                                  ;
                                                                  return Formless_Types_Component.Invalid.value;
                                                                }();

                                                                $138.errors = errors;
                                                                $138.dirty = dirty;
                                                                return $138;
                                                              });
                                                            }

                                                            ;
                                                            var v3 = Formless_Internal_Transform.allTouched()(dictAllTouched)(dictNewtype2)(v1.form);

                                                            if (v3) {
                                                              return Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v4) {
                                                                var $143 = {};

                                                                for (var $144 in v4) {
                                                                  if ({}.hasOwnProperty.call(v4, $144)) {
                                                                    $143[$144] = v4[$144];
                                                                  }

                                                                  ;
                                                                }

                                                                ;

                                                                $143.validity = function () {
                                                                  var $142 = errors === 0;

                                                                  if ($142) {
                                                                    return Formless_Types_Component.Valid.value;
                                                                  }

                                                                  ;
                                                                  return Formless_Types_Component.Invalid.value;
                                                                }();

                                                                $143.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(function (v5) {
                                                                  return {
                                                                    allTouched: true,
                                                                    debounceRef: v5.debounceRef,
                                                                    initialInputs: v5.initialInputs,
                                                                    validationRef: v5.validationRef,
                                                                    validators: v5.validators
                                                                  };
                                                                })(v1.internal);
                                                                $143.errors = errors;
                                                                $143.dirty = dirty;
                                                                return $143;
                                                              });
                                                            }

                                                            ;
                                                            return Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v4) {
                                                              var $146 = {};

                                                              for (var $147 in v4) {
                                                                if ({}.hasOwnProperty.call(v4, $147)) {
                                                                  $146[$147] = v4[$147];
                                                                }

                                                                ;
                                                              }

                                                              ;
                                                              $146.validity = Formless_Types_Component.Incomplete.value;
                                                              $146.errors = errors;
                                                              $146.dirty = dirty;
                                                              return $146;
                                                            });
                                                          }())(function (v2) {
                                                            return handleEvent(Formless_Types_Component.Changed.create(Formless_Internal_Component.getPublicState()(v2)));
                                                          });
                                                        });
                                                      },
                                                      userAction: function userAction(act) {
                                                        return handleAction$prime(act);
                                                      },
                                                      modify: function modify(variant) {
                                                        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                          var $150 = {};

                                                          for (var $151 in st) {
                                                            if ({}.hasOwnProperty.call(st, $151)) {
                                                              $150[$151] = st[$151];
                                                            }

                                                            ;
                                                          }

                                                          ;
                                                          $150.form = Formless_Internal_Transform.unsafeModifyInputVariant(dictNewtype6)(dictNewtype2)(Control_Category.identity(Control_Category.categoryFn))(variant)(st.form);
                                                          return $150;
                                                        }))(function () {
                                                          return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                        });
                                                      },
                                                      validate: function validate(variant) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
                                                          var validators = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v.internal).validators;
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictMonadAff.MonadEffect0().Monad0())(Formless_Internal_Transform.unsafeRunValidationVariant(dictMonadAff.MonadEffect0().Monad0())(dictNewtype7)(dictNewtype2)(dictNewtype4)(variant)(validators)(v.form)))(function (v1) {
                                                            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v2) {
                                                              var $155 = {};

                                                              for (var $156 in v2) {
                                                                if ({}.hasOwnProperty.call(v2, $156)) {
                                                                  $155[$156] = v2[$156];
                                                                }

                                                                ;
                                                              }

                                                              ;
                                                              $155.form = v1;
                                                              return $155;
                                                            }))(function () {
                                                              return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                            });
                                                          });
                                                        });
                                                      },
                                                      modifyValidate: function modifyValidate(v) {
                                                        var validate = Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                                                          var vs = Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v1.internal).validators;
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictMonadAff.MonadEffect0().Monad0())(Formless_Internal_Transform.unsafeRunValidationVariant(dictMonadAff.MonadEffect0().Monad0())(dictNewtype7)(dictNewtype2)(dictNewtype4)(v.value1)(vs)(v1.form)))(function (v2) {
                                                            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                                                              var $161 = {};

                                                              for (var $162 in v3) {
                                                                if ({}.hasOwnProperty.call(v3, $162)) {
                                                                  $161[$162] = v3[$162];
                                                                }

                                                                ;
                                                              }

                                                              ;
                                                              $161.form = v2;
                                                              return $161;
                                                            }))(function () {
                                                              return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v2);
                                                            });
                                                          });
                                                        });

                                                        var modifyWith = function modifyWith(f) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (s) {
                                                            var $164 = {};

                                                            for (var $165 in s) {
                                                              if ({}.hasOwnProperty.call(s, $165)) {
                                                                $164[$165] = s[$165];
                                                              }

                                                              ;
                                                            }

                                                            ;
                                                            $164.form = Formless_Internal_Transform.unsafeModifyInputVariant(dictNewtype6)(dictNewtype2)(f)(v.value1)(s.form);
                                                            return $164;
                                                          }))(function (v1) {
                                                            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v1.form);
                                                          });
                                                        };

                                                        if (v.value0 instanceof Data_Maybe.Nothing) {
                                                          return Control_Apply.applySecond(Halogen_Query_HalogenM.applyHalogenM)(Control_Apply.applySecond(Halogen_Query_HalogenM.applyHalogenM)(modifyWith(Control_Category.identity(Control_Category.categoryFn)))(validate))(_handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync));
                                                        }

                                                        ;

                                                        if (v.value0 instanceof Data_Maybe.Just) {
                                                          return Formless_Internal_Debounce.debounceForm(dictMonadAff)(v.value0.value0)(modifyWith(Control_Category.identity(Control_Category.categoryFn)))(Control_Apply.applySecond(Halogen_Query_HalogenM.applyHalogenM)(modifyWith(Data_Function["const"](Formless_Data_FormFieldResult.Validating.value)))(validate))(_handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync));
                                                        }

                                                        ;
                                                        throw new Error("Failed pattern match at Formless.Component (line 263, column 7 - line 271, column 58): " + [v.value0.constructor.name]);
                                                      },
                                                      reset: function reset(variant) {
                                                        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                          var $172 = {};

                                                          for (var $173 in st) {
                                                            if ({}.hasOwnProperty.call(st, $173)) {
                                                              $172[$173] = st[$173];
                                                            }

                                                            ;
                                                          }

                                                          ;
                                                          $172.form = Formless_Internal_Transform.unsafeModifyInputVariant(dictNewtype6)(dictNewtype2)(Control_Category.identity(Control_Category.categoryFn))(variant)(st.form);
                                                          $172.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(function (v1) {
                                                            return {
                                                              allTouched: false,
                                                              debounceRef: v1.debounceRef,
                                                              initialInputs: v1.initialInputs,
                                                              validationRef: v1.validationRef,
                                                              validators: v1.validators
                                                            };
                                                          })(st.internal);
                                                          return $172;
                                                        }))(function () {
                                                          return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                        });
                                                      },
                                                      setAll: function setAll(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                          var $176 = {};

                                                          for (var $177 in st) {
                                                            if ({}.hasOwnProperty.call(st, $177)) {
                                                              $176[$177] = st[$177];
                                                            }

                                                            ;
                                                          }

                                                          ;
                                                          $176.form = Formless_Internal_Transform.replaceFormFieldInputs()(dictReplaceFormFieldInputs)(dictNewtype)(dictNewtype2)(v.value0)(st.form);
                                                          return $176;
                                                        }))(function (v1) {
                                                          return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(handleEvent(Formless_Types_Component.Changed.create(Formless_Internal_Component.getPublicState()(v1))))(function () {
                                                            if (v.value1) {
                                                              return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(Formless_Action.validateAll);
                                                            }

                                                            ;
                                                            return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                          });
                                                        });
                                                      },
                                                      modifyAll: function modifyAll(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                          var $184 = {};

                                                          for (var $185 in st) {
                                                            if ({}.hasOwnProperty.call(st, $185)) {
                                                              $184[$185] = st[$185];
                                                            }

                                                            ;
                                                          }

                                                          ;
                                                          $184.form = Formless_Internal_Transform.modifyAll()(dictModifyAll)(dictNewtype1)(dictNewtype2)(v.value0)(st.form);
                                                          return $184;
                                                        }))(function (v1) {
                                                          return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(handleEvent(Formless_Types_Component.Changed.create(Formless_Internal_Component.getPublicState()(v1))))(function () {
                                                            if (v.value1) {
                                                              return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(Formless_Action.validateAll);
                                                            }

                                                            ;
                                                            return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                          });
                                                        });
                                                      },
                                                      validateAll: function validateAll(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictMonadAff.MonadEffect0().Monad0())(Formless_Internal_Transform.validateAll()(dictMonadAff.MonadEffect0().Monad0())(dictValidateAll)(dictNewtype4)(dictNewtype2)(Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(v1.internal).validators)(v1.form)))(function (v2) {
                                                            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                                                              var $193 = {};

                                                              for (var $194 in v3) {
                                                                if ({}.hasOwnProperty.call(v3, $194)) {
                                                                  $193[$194] = v3[$194];
                                                                }

                                                                ;
                                                              }

                                                              ;
                                                              $193.form = v2;
                                                              return $193;
                                                            }))(function () {
                                                              return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(sync);
                                                            });
                                                          });
                                                        });
                                                      },
                                                      resetAll: function resetAll(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                                                          var $196 = {};

                                                          for (var $197 in st) {
                                                            if ({}.hasOwnProperty.call(st, $197)) {
                                                              $196[$197] = st[$197];
                                                            }

                                                            ;
                                                          }

                                                          ;
                                                          $196.validity = Formless_Types_Component.Incomplete.value;
                                                          $196.dirty = false;
                                                          $196.errors = 0;
                                                          $196.submitAttempts = 0;
                                                          $196.submitting = false;
                                                          $196.form = Formless_Internal_Transform.replaceFormFieldInputs()(dictReplaceFormFieldInputs)(dictNewtype)(dictNewtype2)(Data_Newtype.unwrap(Formless_Types_Component.newtypeInternalState)(st.internal).initialInputs)(st.form);
                                                          $196.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(function (v2) {
                                                            return {
                                                              allTouched: false,
                                                              debounceRef: v2.debounceRef,
                                                              initialInputs: v2.initialInputs,
                                                              validationRef: v2.validationRef,
                                                              validators: v2.validators
                                                            };
                                                          })(st.internal);
                                                          return $196;
                                                        }))(function (v1) {
                                                          return handleEvent(Formless_Types_Component.Changed.create(Formless_Internal_Component.getPublicState()(v1)));
                                                        });
                                                      },
                                                      submit: function submit(v) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Formless_Internal_Component.preSubmit(dictMonadAff)()(dictAllTouched)(dictSetFormFieldsTouched)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictValidateAll)(dictNewtype2)(dictNewtype3)(dictNewtype4))(function (v1) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(_handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(handleAction$prime)(handleEvent)(Formless_Action.validateAll))(function (v2) {
                                                            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Formless_Internal_Component.submit(dictMonadAff)()(dictAllTouched)(dictSetFormFieldsTouched)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictValidateAll)(dictNewtype2)(dictNewtype3)(dictNewtype4))(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(function ($213) {
                                                              return handleEvent(Formless_Types_Component.Submitted.create($213));
                                                            }));
                                                          });
                                                        });
                                                      },
                                                      loadForm: function loadForm(formInputs) {
                                                        var setFields = function setFields(rec) {
                                                          return {
                                                            allTouched: false,
                                                            initialInputs: formInputs,
                                                            debounceRef: rec.debounceRef,
                                                            validationRef: rec.validationRef,
                                                            validators: rec.validators
                                                          };
                                                        };

                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                                                            var $201 = {};

                                                            for (var $202 in v1) {
                                                              if ({}.hasOwnProperty.call(v1, $202)) {
                                                                $201[$202] = v1[$202];
                                                              }

                                                              ;
                                                            }

                                                            ;
                                                            $201.validity = Formless_Types_Component.Incomplete.value;
                                                            $201.dirty = false;
                                                            $201.errors = 0;
                                                            $201.submitAttempts = 0;
                                                            $201.submitting = false;
                                                            $201.form = Formless_Internal_Transform.replaceFormFieldInputs()(dictReplaceFormFieldInputs)(dictNewtype)(dictNewtype2)(formInputs)(v.form);
                                                            $201.internal = Data_Newtype.over(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.newtypeInternalState)(Formless_Types_Component.InternalState)(setFields)(v.internal);
                                                            return $201;
                                                          }))(function (v1) {
                                                            return handleEvent(Formless_Types_Component.Changed.create(Formless_Internal_Component.getPublicState()(v1)));
                                                          });
                                                        });
                                                      }
                                                    });
                                                  };
                                                };
                                              };
                                            };
                                          };
                                        };
                                      };
                                    };
                                  };
                                };
                              };
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  var _handleQuery = function handleQuery(dictMonadAff) {
    return function (dictRowToList) {
      return function (dictRowToList1) {
        return function (dictEqRecord) {
          return function (dictInputFieldsToFormFields) {
            return function (dictFormFieldsToInputFields) {
              return function (dictCountErrors) {
                return function (dictAllTouched) {
                  return function (dictSetFormFieldsTouched) {
                    return function (dictReplaceFormFieldInputs) {
                      return function (dictModifyAll) {
                        return function (dictValidateAll) {
                          return function (dictFormFieldToMaybeOutput) {
                            return function (dictNewtype) {
                              return function (dictNewtype1) {
                                return function (dictNewtype2) {
                                  return function (dictNewtype3) {
                                    return function (dictNewtype4) {
                                      return function (dictNewtype5) {
                                        return function (dictNewtype6) {
                                          return function (dictNewtype7) {
                                            return function (dictLacks) {
                                              return function (handleQuery$prime) {
                                                return function (handleEvent) {
                                                  return Data_Functor_Variant.match()()()({
                                                    query: function query(v) {
                                                      if (v instanceof Formless_Types_Component.SubmitReply) {
                                                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Formless_Internal_Component.preSubmit(dictMonadAff)()(dictAllTouched)(dictSetFormFieldsTouched)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictValidateAll)(dictNewtype2)(dictNewtype3)(dictNewtype4))(function (v1) {
                                                          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(_handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)))(handleEvent)(Formless_Action.validateAll))(function (v2) {
                                                            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Formless_Internal_Component.submit(dictMonadAff)()(dictAllTouched)(dictSetFormFieldsTouched)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictValidateAll)(dictNewtype2)(dictNewtype3)(dictNewtype4))(function (v3) {
                                                              return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Maybe.Just.create(v.value0(v3)));
                                                            });
                                                          });
                                                        });
                                                      }

                                                      ;

                                                      if (v instanceof Formless_Types_Component.SendQuery) {
                                                        return Halogen_Query_HalogenM.HalogenM(Control_Monad_Free.liftF(new Halogen_Query_HalogenM.ChildQuery(v.value0)));
                                                      }

                                                      ;

                                                      if (v instanceof Formless_Types_Component.AsQuery) {
                                                        return Data_Functor.voidRight(Halogen_Query_HalogenM.functorHalogenM)(new Data_Maybe.Just(v.value1))(_handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)))(handleEvent)(Data_Variant.expand()(v.value0)));
                                                      }

                                                      ;
                                                      throw new Error("Failed pattern match at Formless.Component (line 368, column 12 - line 382, column 44): " + [v.constructor.name]);
                                                    },
                                                    userQuery: function userQuery(q) {
                                                      return handleQuery$prime(q);
                                                    }
                                                  });
                                                };
                                              };
                                            };
                                          };
                                        };
                                      };
                                    };
                                  };
                                };
                              };
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  var defaultSpec = {
    render: Data_Function["const"](Halogen_HTML_Core.text(Data_Monoid.mempty(Data_Monoid.monoidString))),
    handleAction: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)),
    handleQuery: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Maybe.Nothing.value)),
    handleEvent: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)),
    receive: Data_Function["const"](Data_Maybe.Nothing.value),
    initialize: Data_Maybe.Nothing.value,
    finalize: Data_Maybe.Nothing.value
  };

  var component = function component(dictMonadAff) {
    return function (dictRowToList) {
      return function (dictRowToList1) {
        return function (dictEqRecord) {
          return function (dictInputFieldsToFormFields) {
            return function (dictFormFieldsToInputFields) {
              return function (dictCountErrors) {
                return function (dictAllTouched) {
                  return function (dictSetFormFieldsTouched) {
                    return function (dictReplaceFormFieldInputs) {
                      return function (dictModifyAll) {
                        return function (dictValidateAll) {
                          return function (dictFormFieldToMaybeOutput) {
                            return function (dictMakeInputFieldsFromRow) {
                              return function (dictNewtype) {
                                return function (dictNewtype1) {
                                  return function (dictNewtype2) {
                                    return function (dictNewtype3) {
                                      return function (dictNewtype4) {
                                        return function (dictNewtype5) {
                                          return function (dictNewtype6) {
                                            return function (dictNewtype7) {
                                              return function (dictLacks) {
                                                return function (dictLacks1) {
                                                  return function (dictLacks2) {
                                                    return function (dictLacks3) {
                                                      return function (dictLacks4) {
                                                        return function (dictLacks5) {
                                                          return function (dictLacks6) {
                                                            return function (dictLacks7) {
                                                              return function (dictLacks8) {
                                                                return function (mkInput) {
                                                                  return function (spec) {
                                                                    var _initialState = function initialState(input) {
                                                                      var initialInputs = function () {
                                                                        if (input.initialInputs instanceof Data_Maybe.Nothing) {
                                                                          return Formless_Transform_Row.mkInputFields()(dictNewtype)(dictMakeInputFieldsFromRow)(Formless_Types_Form.FormProxy.value);
                                                                        }

                                                                        ;

                                                                        if (input.initialInputs instanceof Data_Maybe.Just) {
                                                                          return input.initialInputs.value0;
                                                                        }

                                                                        ;
                                                                        throw new Error("Failed pattern match at Formless.Component (line 138, column 21 - line 140, column 28): " + [input.initialInputs.constructor.name]);
                                                                      }();

                                                                      var internalState = {
                                                                        allTouched: false,
                                                                        initialInputs: initialInputs,
                                                                        validators: input.validators,
                                                                        debounceRef: Data_Maybe.Nothing.value,
                                                                        validationRef: Data_Maybe.Nothing.value
                                                                      };
                                                                      var initialForm = Formless_Internal_Transform.inputFieldsToFormFields()(dictInputFieldsToFormFields)(dictNewtype)(dictNewtype2)(initialInputs);
                                                                      var pipeline = Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder["delete"](new Data_Symbol.IsSymbol(function () {
                                                                        return "validators";
                                                                      }))()()(Data_Symbol.SProxy.value))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder["delete"](new Data_Symbol.IsSymbol(function () {
                                                                        return "initialInputs";
                                                                      }))()()(Data_Symbol.SProxy.value))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "validity";
                                                                      }))(Data_Symbol.SProxy.value)(Formless_Types_Component.Incomplete.value))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "dirty";
                                                                      }))(Data_Symbol.SProxy.value)(false))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "errors";
                                                                      }))(Data_Symbol.SProxy.value)(0))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "submitAttempts";
                                                                      }))(Data_Symbol.SProxy.value)(0))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "submitting";
                                                                      }))(Data_Symbol.SProxy.value)(false))(Control_Semigroupoid.composeFlipped(Record_Builder.semigroupoidBuilder)(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "form";
                                                                      }))(Data_Symbol.SProxy.value)(initialForm))(Record_Builder.insert()()(new Data_Symbol.IsSymbol(function () {
                                                                        return "internal";
                                                                      }))(Data_Symbol.SProxy.value)(internalState)))))))));
                                                                      return Record_Builder.build(pipeline)(input);
                                                                    };

                                                                    return Halogen_Component.mkComponent({
                                                                      initialState: function initialState($214) {
                                                                        return _initialState(mkInput($214));
                                                                      },
                                                                      render: function () {
                                                                        var $215 = Formless_Internal_Component.getPublicState();
                                                                        return function ($216) {
                                                                          return spec.render($215($216));
                                                                        };
                                                                      }(),
                                                                      "eval": Halogen_Component.mkEval({
                                                                        handleQuery: function handleQuery(q) {
                                                                          return _handleQuery(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(spec.handleQuery)(spec.handleEvent)(q);
                                                                        },
                                                                        handleAction: function handleAction(act) {
                                                                          return _handleAction(dictMonadAff)()()(dictEqRecord)(dictInputFieldsToFormFields)(dictFormFieldsToInputFields)(dictCountErrors)(dictAllTouched)(dictSetFormFieldsTouched)(dictReplaceFormFieldInputs)(dictModifyAll)(dictValidateAll)(dictFormFieldToMaybeOutput)(dictNewtype)(dictNewtype1)(dictNewtype2)(dictNewtype3)(dictNewtype4)(dictNewtype5)(dictNewtype6)(dictNewtype7)()(spec.handleAction)(spec.handleEvent)(act);
                                                                        },
                                                                        initialize: new Data_Maybe.Just(Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
                                                                          return "initialize";
                                                                        }))(Data_Symbol.SProxy.value)(spec.initialize)),
                                                                        receive: Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Formless_Action.injAction))(spec.receive),
                                                                        finalize: Data_Functor.map(Data_Maybe.functorMaybe)(Formless_Action.injAction)(spec.finalize)
                                                                      })
                                                                    });
                                                                  };
                                                                };
                                                              };
                                                            };
                                                          };
                                                        };
                                                      };
                                                    };
                                                  };
                                                };
                                              };
                                            };
                                          };
                                        };
                                      };
                                    };
                                  };
                                };
                              };
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  exports["defaultSpec"] = defaultSpec;
  exports["raiseResult"] = raiseResult;
  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Query"] = $PS["Halogen.Query"] || {};
  var exports = $PS["Halogen.Query"];
  var Data_Unit = $PS["Data.Unit"];

  var tell = function tell(act) {
    return act(Data_Unit.unit);
  };

  exports["tell"] = tell;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Query"] = $PS["Formless.Query"] || {};
  var exports = $PS["Formless.Query"];
  var Data_Functor_Variant = $PS["Data.Functor.Variant"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Halogen_Query = $PS["Halogen.Query"];

  var injQuery = function injQuery(dictFunctor) {
    return Data_Functor_Variant.inj()(new Data_Symbol.IsSymbol(function () {
      return "userQuery";
    }))(dictFunctor)(Data_Symbol.SProxy.value);
  };

  var asQuery = function () {
    var $13 = Data_Functor_Variant.inj()(new Data_Symbol.IsSymbol(function () {
      return "query";
    }))(Formless_Types_Component.functorQueryF)(Data_Symbol.SProxy.value);
    return function ($14) {
      return $13(Halogen_Query.tell(Formless_Types_Component.AsQuery.create($14)));
    };
  }();

  exports["injQuery"] = injQuery;
  exports["asQuery"] = asQuery;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Formless.Retrieve"] = $PS["Formless.Retrieve"] || {};
  var exports = $PS["Formless.Retrieve"];
  var Data_Lens_Fold = $PS["Data.Lens.Fold"];
  var Data_Lens_Getter = $PS["Data.Lens.Getter"];
  var Data_Lens_Internal_Forget = $PS["Data.Lens.Internal.Forget"];
  var Data_Lens_Iso_Newtype = $PS["Data.Lens.Iso.Newtype"];
  var Data_Lens_Record = $PS["Data.Lens.Record"];
  var Data_Maybe_First = $PS["Data.Maybe.First"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Formless_Data_FormFieldResult = $PS["Formless.Data.FormFieldResult"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];

  var _Field = function _Field(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return function (dictStrong) {
            var $94 = Data_Lens_Iso_Newtype["_Newtype"](dictNewtype)(dictNewtype)(dictStrong.Profunctor0());
            var $95 = Data_Lens_Record.prop(dictIsSymbol)()()(sym)(dictStrong);
            var $96 = Data_Lens_Iso_Newtype["_Newtype"](Formless_Types_Form.newtypeFormField)(Formless_Types_Form.newtypeFormField)(dictStrong.Profunctor0());
            return function ($97) {
              return $94($95($96($97)));
            };
          };
        };
      };
    };
  };

  var _FieldInput = function _FieldInput(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return function (dictStrong) {
            var $98 = _Field(dictIsSymbol)(dictNewtype)()(sym)(dictStrong);

            var $99 = Data_Lens_Record.prop(new Data_Symbol.IsSymbol(function () {
              return "input";
            }))()()(Data_Symbol.SProxy.value)(dictStrong);
            return function ($100) {
              return $98($99($100));
            };
          };
        };
      };
    };
  };

  var getInput = function getInput(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return Data_Lens_Getter.view(_FieldInput(dictIsSymbol)(dictNewtype)()(sym)(Data_Lens_Internal_Forget.strongForget));
        };
      };
    };
  };

  var _FieldResult = function _FieldResult(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return function (dictStrong) {
            var $101 = _Field(dictIsSymbol)(dictNewtype)()(sym)(dictStrong);

            var $102 = Data_Lens_Record.prop(new Data_Symbol.IsSymbol(function () {
              return "result";
            }))()()(Data_Symbol.SProxy.value)(dictStrong);
            return function ($103) {
              return $101($102($103));
            };
          };
        };
      };
    };
  };

  var _FieldError = function _FieldError(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return function (dictWander) {
            var $104 = _FieldResult(dictIsSymbol)(dictNewtype)()(sym)(dictWander.Strong0());

            var $105 = Formless_Data_FormFieldResult["_Error"](dictWander.Choice1());
            return function ($106) {
              return $104($105($106));
            };
          };
        };
      };
    };
  };

  var getError = function getError(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictCons) {
        return function (sym) {
          return Data_Lens_Fold.preview(_FieldError(dictIsSymbol)(dictNewtype)()(sym)(Data_Lens_Internal_Forget.wanderForget(Data_Maybe_First.monoidFirst)));
        };
      };
    };
  };

  exports["getInput"] = getInput;
  exports["getError"] = getError;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Aff.Driver.State"] = $PS["Halogen.Aff.Driver.State"] || {};
  var exports = $PS["Halogen.Aff.Driver.State"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var unRenderStateX = Unsafe_Coerce.unsafeCoerce;
  var unDriverStateX = Unsafe_Coerce.unsafeCoerce;

  var renderStateX_ = function renderStateX_(dictApplicative) {
    return function (f) {
      return unDriverStateX(function (st) {
        return Data_Foldable.traverse_(dictApplicative)(Data_Foldable.foldableMaybe)(f)(st.rendering);
      });
    };
  };

  var mkRenderStateX = Unsafe_Coerce.unsafeCoerce;

  var renderStateX = function renderStateX(dictFunctor) {
    return function (f) {
      return unDriverStateX(function (st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };

  var mkDriverStateXRef = Unsafe_Coerce.unsafeCoerce;

  var mapDriverState = function mapDriverState(f) {
    return function (v) {
      return f(v);
    };
  };

  var initDriverState = function initDriverState(component) {
    return function (input) {
      return function (handler) {
        return function (lchs) {
          return function __do() {
            var v = Effect_Ref["new"]({})();
            var v1 = Effect_Ref["new"](Halogen_Data_Slot.empty)();
            var v2 = Effect_Ref["new"](Halogen_Data_Slot.empty)();
            var v3 = Effect_Ref["new"](handler)();
            var v4 = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
            var v5 = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
            var v6 = Effect_Ref["new"](Data_Maybe.Nothing.value)();
            var v7 = Effect_Ref["new"](1)();
            var v8 = Effect_Ref["new"](new Data_Maybe.Just(Data_Map_Internal.empty))();
            var v9 = Effect_Ref["new"](Data_Map_Internal.empty)();
            var ds = {
              component: component,
              state: component.initialState(input),
              refs: Data_Map_Internal.empty,
              children: Halogen_Data_Slot.empty,
              childrenIn: v1,
              childrenOut: v2,
              selfRef: v,
              handlerRef: v3,
              pendingQueries: v4,
              pendingOuts: v5,
              pendingHandlers: v6,
              rendering: Data_Maybe.Nothing.value,
              fresh: v7,
              subscriptions: v8,
              forks: v9,
              lifecycleHandlers: lchs
            };
            Effect_Ref.write(ds)(v)();
            return mkDriverStateXRef(v);
          };
        };
      };
    };
  };

  exports["mapDriverState"] = mapDriverState;
  exports["unDriverStateX"] = unDriverStateX;
  exports["renderStateX"] = renderStateX;
  exports["renderStateX_"] = renderStateX_;
  exports["unRenderStateX"] = unRenderStateX;
  exports["initDriverState"] = initDriverState;
})(PS);

(function (exports) {
  "use strict";

  exports.reallyUnsafeRefEq = function (a) {
    return function (b) {
      return a === b;
    };
  };
})(PS["Unsafe.Reference"] = PS["Unsafe.Reference"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Unsafe.Reference"] = $PS["Unsafe.Reference"] || {};
  var exports = $PS["Unsafe.Reference"];
  var $foreign = $PS["Unsafe.Reference"];
  var unsafeRefEq = $foreign.reallyUnsafeRefEq;
  exports["unsafeRefEq"] = unsafeRefEq;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Aff.Driver.Eval"] = $PS["Halogen.Aff.Driver.Eval"] || {};
  var exports = $PS["Halogen.Aff.Driver.Eval"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Applicative_Free = $PS["Control.Applicative.Free"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Coroutine = $PS["Control.Coroutine"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Fork_Class = $PS["Control.Monad.Fork.Class"];
  var Control_Monad_Free = $PS["Control.Monad.Free"];
  var Control_Monad_Free_Trans = $PS["Control.Monad.Free.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Parallel = $PS["Control.Parallel"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Query_ChildQuery = $PS["Halogen.Query.ChildQuery"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var Unsafe_Reference = $PS["Unsafe.Reference"];

  var unsubscribe = function unsubscribe(sid) {
    return function (ref) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v.subscriptions)))(function (v1) {
          return Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(Halogen_Query_EventSource.finalize)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordSubscriptionId)(sid))(v1));
        });
      });
    };
  };

  var queueOrRun = function queueOrRun(ref) {
    return function (au) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
          return au;
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write(new Data_Maybe.Just(new Data_List_Types.Cons(au, v.value0)))(ref));
        }

        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 195, column 33 - line 197, column 57): " + [v.constructor.name]);
      });
    };
  };

  var handleLifecycle = function handleLifecycle(lchs) {
    return function (f) {
      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
        initializers: Data_List_Types.Nil.value,
        finalizers: Data_List_Types.Nil.value
      })(lchs)))(function () {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(f))(function (v) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(lchs)))(function (v1) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(v1.finalizers))(function () {
              return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Parallel.parSequence_(Effect_Aff.parallelAff)(Data_List_Types.foldableList)(v1.initializers))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v);
              });
            });
          });
        });
      });
    };
  };

  var fresh = function fresh(f) {
    return function (ref) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["modify'"](function (i) {
          return {
            state: i + 1 | 0,
            value: f(i)
          };
        })(v.fresh));
      });
    };
  };

  var evalQ = function evalQ(render) {
    return function (ref) {
      return function (q) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
          return evalM(render)(ref)(v["component"]["eval"](new Halogen_Query_HalogenQ.Query(Data_Functor.map(Data_Coyoneda.functorCoyoneda)(Data_Maybe.Just.create)(Data_Coyoneda.liftCoyoneda(q)), Data_Function["const"](Data_Maybe.Nothing.value))));
        });
      };
    };
  };

  var evalM = function evalM(render) {
    return function (initRef) {
      return function (v) {
        var evalChildQuery = function evalChildQuery(ref) {
          return function (cqb) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
              return Halogen_Query_ChildQuery.unChildQueryBox(function (v2) {
                var evalChild = function evalChild(v3) {
                  return Control_Parallel_Class.parallel(Effect_Aff.parallelAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v3)))(function (v4) {
                    return Halogen_Aff_Driver_State.unDriverStateX(function (ds) {
                      return evalQ(render)(ds.selfRef)(v2.value1);
                    })(v4);
                  }));
                };

                return Data_Functor.map(Effect_Aff.functorAff)(v2.value2)(Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(v2.value0(Effect_Aff.applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };

        var go = function go(ref) {
          return function (v1) {
            if (v1 instanceof Halogen_Query_HalogenM.State) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                var v3 = v1.value0(v2.state);

                if (Unsafe_Reference.unsafeRefEq(v2.state)(v3.value1)) {
                  return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                }

                ;

                if (Data_Boolean.otherwise) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(handleLifecycle(v2.lifecycleHandlers)(render(v2.lifecycleHandlers)(ref)))(function () {
                      return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                    });
                  });
                }

                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 85, column 7 - line 91, column 21): " + [v3.constructor.name]);
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Subscribe) {
              return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.SubscriptionId)(ref))(function (v2) {
                var v3 = v1.value0(v2);
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v4) {
                  return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Control_Bind.bind(Effect_Aff.bindAff)(v3)(function (v5) {
                    var done = Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v4.subscriptions)))(function (v6) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal["delete"](Halogen_Query_HalogenM.ordSubscriptionId)(v2)))(v4.subscriptions)))(function () {
                        return Control_Applicative.when(Effect_Aff.applicativeAff)(Data_Maybe.maybe(false)(Data_Map_Internal.member(Halogen_Query_HalogenM.ordSubscriptionId)(v2))(v6))(Halogen_Query_EventSource.finalize(v5.finalizer));
                      });
                    });
                    var consumer = Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Coroutine["await"](Effect_Aff.monadAff))(function (v6) {
                      return Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Effect_Aff.monadAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v4.subscriptions))))(function (v7) {
                        return Control_Applicative.when(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqBoolean))(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.member(Halogen_Query_HalogenM.ordSubscriptionId)(v2))(v7))(new Data_Maybe.Just(true)))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Effect_Aff.monadAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(evalF(render)(ref)(new Halogen_Query_Input.Action(v6)))))(function (v8) {
                          return consumer;
                        }));
                      });
                    });
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordSubscriptionId)(v2)(done)))(v4.subscriptions)))(function () {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Coroutine.runProcess(Effect_Aff.monadRecAff)(Control_Coroutine.pullFrom(Effect_Aff.monadRecAff)(consumer)(v5.producer)))(function () {
                        return Halogen_Query_EventSource.finalize(done);
                      });
                    });
                  })))(function (v5) {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(v2));
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Unsubscribe) {
              return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(unsubscribe(v1.value0)(ref))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Lift) {
              return v1.value0;
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.ChildQuery) {
              return evalChildQuery(ref)(v1.value0);
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Raise) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.handlerRef)))(function (v3) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(queueOrRun(v2.pendingOuts)(v3(v1.value0)))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Par) {
              return Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(Control_Applicative_Free.retractFreeAp(Effect_Aff.applicativeParAff)(Control_Applicative_Free.hoistFreeAp(function () {
                var $111 = Control_Parallel_Class.parallel(Effect_Aff.parallelAff);
                var $112 = evalM(render)(ref);
                return function ($113) {
                  return $111($112($113));
                };
              }())(v1.value0)));
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Fork) {
              return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.ForkId)(ref))(function (v2) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v3) {
                  return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](false)))(function (v4) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Effect_Aff["finally"](Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                      Effect_Ref.modify_(Data_Map_Internal["delete"](Halogen_Query_HalogenM.ordForkId)(v2))(v3.forks)();
                      return Effect_Ref.write(true)(v4)();
                    }))(evalM(render)(ref)(v1.value0))))(function (v5) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Monad.unlessM(Effect.monadEffect)(Effect_Ref.read(v4))(Effect_Ref.modify_(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordForkId)(v2)(v5))(v3.forks))))(function () {
                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(v2));
                      });
                    });
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Kill) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.forks)))(function (v3) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(Effect_Aff.killFiber(Effect_Exception.error("Cancelled")))(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordForkId)(v1.value0)(v3)))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.GetRef) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(Data_Map_Internal.lookup(Data_Ord.ordString)(v1.value0)(v2.refs)));
              });
            }

            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 82, column 12 - line 146, column 33): " + [v1.constructor.name]);
          };
        };

        return Control_Monad_Free.foldFree(Effect_Aff.monadRecAff)(go(initRef))(v);
      };
    };
  };

  var evalF = function evalF(render) {
    return function (ref) {
      return function (v) {
        if (v instanceof Halogen_Query_Input.RefUpdate) {
          return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Function.flip(Effect_Ref.modify_)(ref)(Halogen_Aff_Driver_State.mapDriverState(function (st) {
            return {
              component: st.component,
              state: st.state,
              refs: Data_Map_Internal.alter(Data_Ord.ordString)(Data_Function["const"](v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }

        ;

        if (v instanceof Halogen_Query_Input.Action) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
            return evalM(render)(ref)(v1["component"]["eval"](new Halogen_Query_HalogenQ.Action(v.value0, Data_Unit.unit)));
          });
        }

        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 51, column 20 - line 57, column 62): " + [v.constructor.name]);
      };
    };
  };

  exports["evalF"] = evalF;
  exports["evalQ"] = evalQ;
  exports["evalM"] = evalM;
  exports["handleLifecycle"] = handleLifecycle;
  exports["queueOrRun"] = queueOrRun;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Aff.Driver"] = $PS["Halogen.Aff.Driver"] || {};
  var exports = $PS["Halogen.Aff.Driver"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Coroutine = $PS["Control.Coroutine"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Fork_Class = $PS["Control.Monad.Fork.Class"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Parallel = $PS["Control.Parallel"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_AVar = $PS["Effect.Aff.AVar"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Console = $PS["Effect.Console"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver_Eval = $PS["Halogen.Aff.Driver.Eval"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var newLifecycleHandlers = Effect_Ref["new"]({
    initializers: Data_List_Types.Nil.value,
    finalizers: Data_List_Types.Nil.value
  });
  var handleAff = Effect_Aff.runAff_(Data_Either.either(Effect_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))));

  var handlePending = function handlePending(ref) {
    return function __do() {
      var v = Effect_Ref.read(ref)();
      Effect_Ref.write(Data_Maybe.Nothing.value)(ref)();
      return Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v)(function () {
        var $79 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff));
        return function ($80) {
          return handleAff($79(Data_List.reverse($80)));
        };
      }())();
    };
  };

  var cleanupSubscriptionsAndForks = function cleanupSubscriptionsAndForks(v) {
    return function __do() {
      Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function () {
        var $81 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Map_Internal.foldableMap)(function () {
          var $83 = Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff);
          return function ($84) {
            return $83(Halogen_Query_EventSource.finalize($84));
          };
        }());
        return function ($82) {
          return handleAff($81($82));
        };
      }()))(Effect_Ref.read(v.subscriptions))();
      Effect_Ref.write(Data_Maybe.Nothing.value)(v.subscriptions)();
      Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Map_Internal.foldableMap)(function () {
        var $85 = Effect_Aff.killFiber(Effect_Exception.error("finalized"));
        return function ($86) {
          return handleAff($85($86));
        };
      }()))(Effect_Ref.read(v.forks))();
      return Effect_Ref.write(Data_Map_Internal.empty)(v.forks)();
    };
  };

  var runUI = function runUI(renderSpec) {
    return function (component) {
      return function (i) {
        var subscribe = function subscribe(fresh) {
          return function (ref) {
            return function (consumer) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.empty)(function (v) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                  var v1 = Effect_Ref.read(fresh)();
                  Effect_Ref.modify_(function (v2) {
                    return v2 + 1 | 0;
                  })(fresh)();
                  Effect_Ref.modify_(Data_Map_Internal.insert(Data_Ord.ordInt)(v1)(v))(ref)();
                  return v1;
                }))(function (v1) {
                  var producer = Control_Coroutine.producer(Effect_Aff.monadAff)(Data_Functor.map(Effect_Aff.functorAff)(Data_Either.either(Data_Function["const"](new Data_Either.Right(Data_Unit.unit)))(Data_Either.Left.create))(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(Effect_Aff_AVar.take(v))));
                  return Data_Functor["void"](Effect_Aff.functorAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Coroutine.runProcess(Effect_Aff.monadRecAff)(Control_Coroutine.connect(Effect_Aff.monadRecAff)(Effect_Aff.parallelAff)(producer)(consumer)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Map_Internal["delete"](Data_Ord.ordInt)(v1))(ref)))(function () {
                      return Effect_Aff_AVar.kill(Effect_Exception.error("ended"))(v);
                    });
                  })));
                });
              });
            };
          };
        };

        var rootHandler = function rootHandler(ref) {
          return function (message) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
              return Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Map_Internal.foldableMap)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(Data_Functor.map(Data_Map_Internal.functorMap)(Effect_Aff_AVar.put(message))(v));
            });
          };
        };

        var squashChildInitializers = function squashChildInitializers(lchs) {
          return function (preInits) {
            return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
              var parentInitializer = Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](new Halogen_Query_HalogenQ.Initialize(Data_Unit.unit)));
              return Effect_Ref.modify_(function (handlers) {
                return {
                  initializers: new Data_List_Types.Cons(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Parallel.parSequence_(Effect_Aff.parallelAff)(Data_List_Types.foldableList)(Data_List.reverse(handlers.initializers)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(parentInitializer)(function () {
                      return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };

        var runComponent = function runComponent(lchs) {
          return function (handler) {
            return function (j) {
              return Halogen_Component.unComponent(function (c) {
                return function __do() {
                  var v = newLifecycleHandlers();
                  var v1 = Halogen_Aff_Driver_State.initDriverState(c)(j)(handler)(v)();
                  var v2 = Effect_Ref.read(lchs)();
                  Effect_Ref.write({
                    initializers: Data_List_Types.Nil.value,
                    finalizers: v2.finalizers
                  })(lchs)();
                  Control_Bind.bindFlipped(Effect.bindEffect)(Halogen_Aff_Driver_State.unDriverStateX(function () {
                    var $87 = render(lchs);
                    return function ($88) {
                      return $87(function (v3) {
                        return v3.selfRef;
                      }($88));
                    };
                  }()))(Effect_Ref.read(v1))();
                  Control_Bind.bindFlipped(Effect.bindEffect)(squashChildInitializers(lchs)(v2.initializers))(Effect_Ref.read(v1))();
                  return v1;
                };
              });
            };
          };
        };

        var renderChild = function renderChild(lchs) {
          return function (handler) {
            return function (childrenInRef) {
              return function (childrenOutRef) {
                return Halogen_Component.unComponentSlot(function (slot) {
                  return function __do() {
                    var v = Effect_Ref.read(childrenInRef)();

                    var v1 = function () {
                      var v1 = slot.pop(v);

                      if (v1 instanceof Data_Maybe.Just) {
                        Effect_Ref.write(v1.value0.value1)(childrenInRef)();
                        var v2 = Effect_Ref.read(v1.value0.value0)();
                        Halogen_Aff_Driver_State.unDriverStateX(function (st) {
                          return function __do() {
                            Data_Function.flip(Effect_Ref.write)(st.handlerRef)(function () {
                              var $89 = Data_Maybe.maybe(Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit))(handler);
                              return function ($90) {
                                return $89(slot.output($90));
                              };
                            }())();
                            return handleAff(Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](slot.input)))();
                          };
                        })(v2)();
                        return v1.value0.value0;
                      }

                      ;

                      if (v1 instanceof Data_Maybe.Nothing) {
                        if (slot.input instanceof Halogen_Query_HalogenQ.Receive) {
                          return runComponent(lchs)(function () {
                            var $91 = Data_Maybe.maybe(Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit))(handler);
                            return function ($92) {
                              return $91(slot.output($92));
                            };
                          }())(slot.input.value0)(slot.component)();
                        }

                        ;
                        return Effect_Exception["throw"]("Halogen internal error: slot input was not a Receive query")();
                      }

                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 236, column 14 - line 249, column 81): " + [v1.constructor.name]);
                    }();

                    var v2 = Data_Functor.map(Effect.functorEffect)(function ($93) {
                      return Data_Maybe.isJust(slot.get($93));
                    })(Effect_Ref.read(childrenOutRef))();
                    Control_Applicative.when(Effect.applicativeEffect)(v2)(Effect_Console.warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    Effect_Ref.modify_(slot.set(v1))(childrenOutRef)();
                    return Control_Bind.bind(Effect.bindEffect)(Effect_Ref.read(v1))(Halogen_Aff_Driver_State.renderStateX(Effect.functorEffect)(function (v3) {
                      if (v3 instanceof Data_Maybe.Nothing) {
                        return Effect_Exception["throw"]("Halogen internal error: child was not initialized in renderChild");
                      }

                      ;

                      if (v3 instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure(Effect.applicativeEffect)(renderSpec.renderChild(v3.value0));
                      }

                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 254, column 37 - line 256, column 50): " + [v3.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };

        var render = function render(lchs) {
          return function ($$var) {
            return function __do() {
              var v = Effect_Ref.read($$var)();
              var v1 = Data_Functor.map(Effect.functorEffect)(Data_Maybe.isNothing)(Effect_Ref.read(v.pendingHandlers))();
              Control_Applicative.when(Effect.applicativeEffect)(v1)(Effect_Ref.write(new Data_Maybe.Just(Data_List_Types.Nil.value))(v.pendingHandlers))();
              Effect_Ref.write(Halogen_Data_Slot.empty)(v.childrenOut)();
              Effect_Ref.write(v.children)(v.childrenIn)();
              var selfRef = Control_Category.identity(Control_Category.categoryFn)(v.selfRef);
              var pendingQueries = Control_Category.identity(Control_Category.categoryFn)(v.pendingQueries);
              var pendingHandlers = Control_Category.identity(Control_Category.categoryFn)(v.pendingHandlers);

              var handler = function () {
                var $94 = Halogen_Aff_Driver_Eval.queueOrRun(pendingHandlers);
                var $95 = Data_Functor["void"](Effect_Aff.functorAff);
                var $96 = Halogen_Aff_Driver_Eval.evalF(render)(selfRef);
                return function ($97) {
                  return $94($95($96($97)));
                };
              }();

              var childHandler = function () {
                var $98 = Halogen_Aff_Driver_Eval.queueOrRun(pendingQueries);
                return function ($99) {
                  return $98(handler(Halogen_Query_Input.Action.create($99)));
                };
              }();

              var v2 = renderSpec.render(function ($100) {
                return handleAff(handler($100));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var v3 = Effect_Ref.read(v.childrenOut)();
              var v4 = Effect_Ref.read(v.childrenIn)();
              Halogen_Data_Slot.foreachSlot(Effect.applicativeEffect)(v4)(function (v5) {
                return function __do() {
                  var v6 = Effect_Ref.read(v5)();
                  Halogen_Aff_Driver_State.renderStateX_(Effect.applicativeEffect)(renderSpec.removeChild)(v6)();
                  return finalize(lchs)(v6)();
                };
              })();
              Data_Function.flip(Effect_Ref.modify_)(v.selfRef)(Halogen_Aff_Driver_State.mapDriverState(function (ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: v3,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Data_Maybe.Just(v2),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return Control_Applicative.when(Effect.applicativeEffect)(v1)(Data_Function.flip(Control_Monad_Rec_Class.tailRecM(Control_Monad_Rec_Class.monadRecEffect))(Data_Unit.unit)(function (v5) {
                return function __do() {
                  var v6 = Effect_Ref.read(v.pendingHandlers)();
                  Effect_Ref.write(new Data_Maybe.Just(Data_List_Types.Nil.value))(v.pendingHandlers)();
                  Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function () {
                    var $101 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff));
                    return function ($102) {
                      return handleAff($101(Data_List.reverse($102)));
                    };
                  }())(v6)();
                  var v7 = Effect_Ref.read(v.pendingHandlers)();
                  var $69 = Data_Maybe.maybe(false)(Data_List["null"])(v7);

                  if ($69) {
                    return Data_Functor.voidLeft(Effect.functorEffect)(Effect_Ref.write(Data_Maybe.Nothing.value)(v.pendingHandlers))(new Control_Monad_Rec_Class.Done(Data_Unit.unit))();
                  }

                  ;
                  return new Control_Monad_Rec_Class.Loop(Data_Unit.unit);
                };
              }))();
            };
          };
        };

        var finalize = function finalize(lchs) {
          return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
            return function __do() {
              cleanupSubscriptionsAndForks(st)();
              var f = Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](new Halogen_Query_HalogenQ.Finalize(Data_Unit.unit)));
              Effect_Ref.modify_(function (handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Data_List_Types.Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return Halogen_Data_Slot.foreachSlot(Effect.applicativeEffect)(st.children)(function (v) {
                return function __do() {
                  var v1 = Effect_Ref.read(v)();
                  return finalize(lchs)(v1)();
                };
              })();
            };
          });
        };

        var evalDriver = function evalDriver(disposed) {
          return function (ref) {
            return function (q) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(disposed)))(function (v) {
                if (v) {
                  return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Maybe.Nothing.value);
                }

                ;
                return Halogen_Aff_Driver_Eval.evalQ(render)(ref)(q);
              });
            };
          };
        };

        var dispose = function dispose(disposed) {
          return function (lchs) {
            return function (dsx) {
              return function (subsRef) {
                return Halogen_Aff_Driver_Eval.handleLifecycle(lchs)(function __do() {
                  var v = Effect_Ref.read(disposed)();

                  if (v) {
                    return Data_Unit.unit;
                  }

                  ;
                  Effect_Ref.write(true)(disposed)();
                  Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Map_Internal.foldableMap)(function () {
                    var $103 = Effect_Aff_AVar.kill(Effect_Exception.error("disposed"));
                    return function ($104) {
                      return Effect_Aff.launchAff_($103($104));
                    };
                  }()))(Effect_Ref.read(subsRef))();
                  finalize(lchs)(dsx)();
                  return Halogen_Aff_Driver_State.unDriverStateX(function () {
                    var $105 = Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(renderSpec.dispose);
                    return function ($106) {
                      return $105(function (v1) {
                        return v1.rendering;
                      }($106));
                    };
                  }())(dsx)();
                });
              };
            };
          };
        };

        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(newLifecycleHandlers))(function (v) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](0)))(function (v1) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](false)))(function (v2) {
              return Halogen_Aff_Driver_Eval.handleLifecycle(v)(function __do() {
                var v3 = Effect_Ref["new"](Data_Map_Internal.empty)();
                var v4 = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref.read)(runComponent(v)(rootHandler(v3))(i)(component))();
                return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
                  return Control_Applicative.pure(Effect.applicativeEffect)({
                    query: evalDriver(v2)(st.selfRef),
                    subscribe: subscribe(v1)(v3),
                    dispose: dispose(v2)(v)(v4)(v3)
                  });
                })(v4)();
              });
            });
          });
        });
      };
    };
  };

  exports["runUI"] = runUI;
})(PS);

(function (exports) {
  "use strict";

  exports._querySelector = function (selector) {
    return function (node) {
      return function () {
        return node.querySelector(selector);
      };
    };
  };
})(PS["Web.DOM.ParentNode"] = PS["Web.DOM.ParentNode"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.DOM.ParentNode"] = $PS["Web.DOM.ParentNode"] || {};
  var exports = $PS["Web.DOM.ParentNode"];
  var $foreign = $PS["Web.DOM.ParentNode"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];

  var querySelector = function querySelector(qs) {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $4 = $foreign["_querySelector"](qs);
    return function ($5) {
      return $3($4($5));
    };
  };

  exports["querySelector"] = querySelector;
})(PS);

(function (exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML"] = $PS["Web.HTML"] || {};
  var exports = $PS["Web.HTML"];
  var $foreign = $PS["Web.HTML"];
  exports["window"] = $foreign.window;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.Event.EventTypes"] = $PS["Web.HTML.Event.EventTypes"] || {};
  var exports = $PS["Web.HTML.Event.EventTypes"];
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";
  exports["domcontentloaded"] = domcontentloaded;
  exports["input"] = input;
})(PS);

(function (exports) {
  "use strict";

  exports._readyState = function (doc) {
    return function () {
      return doc.readyState;
    };
  };
})(PS["Web.HTML.HTMLDocument"] = PS["Web.HTML.HTMLDocument"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.HTMLDocument.ReadyState"] = $PS["Web.HTML.HTMLDocument.ReadyState"] || {};
  var exports = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Loading = function () {
    function Loading() {}

    ;
    Loading.value = new Loading();
    return Loading;
  }();

  var Interactive = function () {
    function Interactive() {}

    ;
    Interactive.value = new Interactive();
    return Interactive;
  }();

  var Complete = function () {
    function Complete() {}

    ;
    Complete.value = new Complete();
    return Complete;
  }();

  var parse = function parse(v) {
    if (v === "loading") {
      return new Data_Maybe.Just(Loading.value);
    }

    ;

    if (v === "interactive") {
      return new Data_Maybe.Just(Interactive.value);
    }

    ;

    if (v === "complete") {
      return new Data_Maybe.Just(Complete.value);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  exports["Loading"] = Loading;
  exports["parse"] = parse;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.HTMLDocument"] = $PS["Web.HTML.HTMLDocument"] || {};
  var exports = $PS["Web.HTML.HTMLDocument"];
  var $foreign = $PS["Web.HTML.HTMLDocument"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect = $PS["Effect"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_HTML_HTMLDocument_ReadyState = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var toParentNode = Unsafe_Coerce.unsafeCoerce;
  var toDocument = Unsafe_Coerce.unsafeCoerce;

  var readyState = function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(function () {
      var $2 = Data_Maybe.fromMaybe(Web_HTML_HTMLDocument_ReadyState.Loading.value);
      return function ($3) {
        return $2(Web_HTML_HTMLDocument_ReadyState.parse($3));
      };
    }());
    return function ($1) {
      return $0($foreign["_readyState"]($1));
    };
  }();

  exports["toDocument"] = toDocument;
  exports["toParentNode"] = toParentNode;
  exports["readyState"] = readyState;
})(PS);

(function (exports) {
  "use strict";

  exports._read = function (nothing, just, value) {
    var tag = Object.prototype.toString.call(value);

    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value);
    } else {
      return nothing;
    }
  };
})(PS["Web.HTML.HTMLElement"] = PS["Web.HTML.HTMLElement"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.HTMLElement"] = $PS["Web.HTML.HTMLElement"] || {};
  var exports = $PS["Web.HTML.HTMLElement"];
  var $foreign = $PS["Web.HTML.HTMLElement"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toNode = Unsafe_Coerce.unsafeCoerce;

  var fromElement = function fromElement(x) {
    return $foreign["_read"](Data_Maybe.Nothing.value, Data_Maybe.Just.create, x);
  };

  exports["fromElement"] = fromElement;
  exports["toNode"] = toNode;
})(PS);

(function (exports) {
  "use strict";

  exports.document = function (window) {
    return function () {
      return window.document;
    };
  };

  exports.location = function (window) {
    return function () {
      return window.location;
    };
  };

  exports.localStorage = function (window) {
    return function () {
      return window.localStorage;
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.Window"] = $PS["Web.HTML.Window"] || {};
  var exports = $PS["Web.HTML.Window"];
  var $foreign = $PS["Web.HTML.Window"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toEventTarget = Unsafe_Coerce.unsafeCoerce;
  exports["toEventTarget"] = toEventTarget;
  exports["document"] = $foreign.document;
  exports["location"] = $foreign.location;
  exports["localStorage"] = $foreign.localStorage;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.Aff.Util"] = $PS["Halogen.Aff.Util"] || {};
  var exports = $PS["Halogen.Aff.Util"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Web_DOM_ParentNode = $PS["Web.DOM.ParentNode"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_Event_EventTypes = $PS["Web.HTML.Event.EventTypes"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_HTMLDocument_ReadyState = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var Web_HTML_HTMLElement = $PS["Web.HTML.HTMLElement"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];

  var selectElement = function selectElement(query) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Bind.bindFlipped(Effect.bindEffect)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)(function () {
      var $12 = Web_DOM_ParentNode.querySelector(query);
      return function ($13) {
        return $12(Web_HTML_HTMLDocument.toParentNode($13));
      };
    }())(Web_HTML_Window.document))(Web_HTML.window)))(function (v) {
      return Control_Applicative.pure(Effect_Aff.applicativeAff)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Web_HTML_HTMLElement.fromElement)(v));
    });
  };

  var runHalogenAff = Effect_Aff.runAff_(Data_Either.either(Effect_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))));
  var awaitLoad = Effect_Aff.makeAff(function (callback) {
    return function __do() {
      var v = Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_HTMLDocument.readyState)(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))();

      if (v instanceof Web_HTML_HTMLDocument_ReadyState.Loading) {
        var v1 = Data_Functor.map(Effect.functorEffect)(Web_HTML_Window.toEventTarget)(Web_HTML.window)();
        var v2 = Web_Event_EventTarget.eventListener(function (v2) {
          return callback(new Data_Either.Right(Data_Unit.unit));
        })();
        Web_Event_EventTarget.addEventListener(Web_HTML_Event_EventTypes.domcontentloaded)(v2)(false)(v1)();
        return Effect_Aff.effectCanceler(Web_Event_EventTarget.removeEventListener(Web_HTML_Event_EventTypes.domcontentloaded)(v2)(false)(v1));
      }

      ;
      callback(new Data_Either.Right(Data_Unit.unit))();
      return Effect_Aff.nonCanceler;
    };
  });
  var awaitBody = Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(awaitLoad)(function () {
    return Control_Bind.bind(Effect_Aff.bindAff)(selectElement("body"))(function (v) {
      return Data_Maybe.maybe(Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff)(Effect_Exception.error("Could not find body")))(Control_Applicative.pure(Effect_Aff.applicativeAff))(v);
    });
  });
  exports["awaitBody"] = awaitBody;
  exports["runHalogenAff"] = runHalogenAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.HTML.Elements"] = $PS["Halogen.HTML.Elements"] || {};
  var exports = $PS["Halogen.HTML.Elements"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var element = Halogen_HTML_Core.element(Data_Maybe.Nothing.value);
  var fieldset = element("fieldset");
  var fieldset_ = fieldset([]);
  var footer = element("footer");
  var form = element("form");
  var form_ = form([]);
  var h1 = element("h1");
  var h3 = element("h3");
  var h4 = element("h4");
  var h4_ = h4([]);

  var hr = function hr(props) {
    return element("hr")(props)([]);
  };

  var hr_ = hr([]);
  var i = element("i");

  var img = function img(props) {
    return element("img")(props)([]);
  };

  var input = function input(props) {
    return element("input")(props)([]);
  };

  var li = element("li");
  var nav = element("nav");
  var p = element("p");
  var p_ = p([]);
  var span = element("span");

  var textarea = function textarea(es) {
    return element("textarea")(es)([]);
  };

  var ul = element("ul");
  var div = element("div");
  var div_ = div([]);
  var button = element("button");
  var a = element("a");
  exports["a"] = a;
  exports["button"] = button;
  exports["div"] = div;
  exports["div_"] = div_;
  exports["fieldset"] = fieldset;
  exports["fieldset_"] = fieldset_;
  exports["footer"] = footer;
  exports["form_"] = form_;
  exports["h1"] = h1;
  exports["h3"] = h3;
  exports["h4_"] = h4_;
  exports["hr_"] = hr_;
  exports["i"] = i;
  exports["img"] = img;
  exports["input"] = input;
  exports["li"] = li;
  exports["nav"] = nav;
  exports["p"] = p;
  exports["p_"] = p_;
  exports["span"] = span;
  exports["textarea"] = textarea;
  exports["ul"] = ul;
})(PS);

(function (exports) {
  "use strict";

  exports._currentTarget = function (e) {
    return e.currentTarget;
  };
})(PS["Web.Event.Event"] = PS["Web.Event.Event"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.Event.Event"] = $PS["Web.Event.Event"] || {};
  var exports = $PS["Web.Event.Event"];
  var $foreign = $PS["Web.Event.Event"];
  var Data_Nullable = $PS["Data.Nullable"];

  var currentTarget = function currentTarget($8) {
    return Data_Nullable.toMaybe($foreign["_currentTarget"]($8));
  };

  exports["currentTarget"] = currentTarget;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.UIEvent.MouseEvent.EventTypes"] = $PS["Web.UIEvent.MouseEvent.EventTypes"] || {};
  var exports = $PS["Web.UIEvent.MouseEvent.EventTypes"];
  var click = "click";
  exports["click"] = click;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.HTML.Events"] = $PS["Halogen.HTML.Events"] || {};
  var exports = $PS["Halogen.HTML.Events"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Foreign = $PS["Foreign"];
  var Foreign_Index = $PS["Foreign.Index"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_Event_Event = $PS["Web.Event.Event"];
  var Web_HTML_Event_EventTypes = $PS["Web.HTML.Event.EventTypes"];
  var Web_UIEvent_MouseEvent_EventTypes = $PS["Web.UIEvent.MouseEvent.EventTypes"];
  var mouseHandler = Unsafe_Coerce.unsafeCoerce;

  var handler = function handler(et) {
    var $0 = Halogen_HTML_Core.handler(et);
    var $1 = Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Halogen_Query_Input.Action.create));
    return function ($2) {
      return $0($1($2));
    };
  };

  var onClick = function () {
    var $3 = handler(Web_UIEvent_MouseEvent_EventTypes.click);
    return function ($4) {
      return $3(mouseHandler($4));
    };
  }();

  var addForeignPropHandler = function addForeignPropHandler(key) {
    return function (prop) {
      return function (reader) {
        return function (f) {
          var go = function go(a) {
            return Control_Bind.composeKleisliFlipped(Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity))(reader)(Foreign_Index.readProp(prop))(Foreign.unsafeToForeign(a));
          };

          return handler(key)(Control_Bind.composeKleisli(Data_Maybe.bindMaybe)(Web_Event_Event.currentTarget)(function (e) {
            return Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(f)(Control_Monad_Except.runExcept(go(e)));
          }));
        };
      };
    };
  };

  var onValueInput = addForeignPropHandler(Web_HTML_Event_EventTypes.input)("value")(Foreign.readString);
  exports["onClick"] = onClick;
  exports["onValueInput"] = onValueInput;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.HTML.Properties"] = $PS["Halogen.HTML.Properties"] || {};
  var exports = $PS["Halogen.HTML.Properties"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];

  var prop = function prop(dictIsProp) {
    return Halogen_HTML_Core.prop(dictIsProp);
  };

  var rows = prop(Halogen_HTML_Core.isPropInt)("rows");
  var src = prop(Halogen_HTML_Core.isPropString)("src");

  var type_ = function type_(dictIsProp) {
    return prop(dictIsProp)("type");
  };

  var value = prop(Halogen_HTML_Core.isPropString)("value");
  var placeholder = prop(Halogen_HTML_Core.isPropString)("placeholder");
  var id_ = prop(Halogen_HTML_Core.isPropString)("id");
  var href = prop(Halogen_HTML_Core.isPropString)("href");

  var class_ = function () {
    var $18 = prop(Halogen_HTML_Core.isPropString)("className");
    var $19 = Data_Newtype.unwrap(Halogen_HTML_Core.newtypeClassName);
    return function ($20) {
      return $18($19($20));
    };
  }();

  var attr = Halogen_HTML_Core.attr(Data_Maybe.Nothing.value);
  exports["attr"] = attr;
  exports["class_"] = class_;
  exports["rows"] = rows;
  exports["href"] = href;
  exports["id_"] = id_;
  exports["src"] = src;
  exports["type_"] = type_;
  exports["value"] = value;
  exports["placeholder"] = placeholder;
})(PS);

(function (exports) {
  "use strict";

  var getEffProp = function getEffProp(name) {
    return function (node) {
      return function () {
        return node[name];
      };
    };
  };

  exports._parentNode = getEffProp("parentNode");
  exports._nextSibling = getEffProp("nextSibling");

  exports.insertBefore = function (node1) {
    return function (node2) {
      return function (parent) {
        return function () {
          return parent.insertBefore(node1, node2);
        };
      };
    };
  };

  exports.appendChild = function (node) {
    return function (parent) {
      return function () {
        return parent.appendChild(node);
      };
    };
  };

  exports.removeChild = function (node) {
    return function (parent) {
      return function () {
        return parent.removeChild(node);
      };
    };
  };
})(PS["Web.DOM.Node"] = PS["Web.DOM.Node"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.DOM.Node"] = $PS["Web.DOM.Node"] || {};
  var exports = $PS["Web.DOM.Node"];
  var $foreign = $PS["Web.DOM.Node"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];

  var parentNode = function () {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($4) {
      return $3($foreign["_parentNode"]($4));
    };
  }();

  var nextSibling = function () {
    var $14 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($15) {
      return $14($foreign["_nextSibling"]($15));
    };
  }();

  exports["parentNode"] = parentNode;
  exports["nextSibling"] = nextSibling;
  exports["insertBefore"] = $foreign.insertBefore;
  exports["appendChild"] = $foreign.appendChild;
  exports["removeChild"] = $foreign.removeChild;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Halogen.VDom.Driver"] = $PS["Halogen.VDom.Driver"] || {};
  var exports = $PS["Halogen.VDom.Driver"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver = $PS["Halogen.Aff.Driver"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_VDom_DOM = $PS["Halogen.VDom.DOM"];
  var Halogen_VDom_DOM_Prop = $PS["Halogen.VDom.DOM.Prop"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Thunk = $PS["Halogen.VDom.Thunk"];
  var Unsafe_Reference = $PS["Unsafe.Reference"];
  var Web_DOM_Node = $PS["Web.DOM.Node"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_HTMLElement = $PS["Web.HTML.HTMLElement"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];

  var substInParent = function substInParent(v) {
    return function (v1) {
      return function (v2) {
        if (v1 instanceof Data_Maybe.Just && v2 instanceof Data_Maybe.Just) {
          return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.insertBefore(v)(v1.value0)(v2.value0));
        }

        ;

        if (v1 instanceof Data_Maybe.Nothing && v2 instanceof Data_Maybe.Just) {
          return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(v)(v2.value0));
        }

        ;
        return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
      };
    };
  };

  var removeChild = function removeChild(v) {
    return function __do() {
      var v1 = Web_DOM_Node.parentNode(v.node)();
      return Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function (pn) {
        return Web_DOM_Node.removeChild(v.node)(pn);
      })(v1)();
    };
  };

  var mkSpec = function mkSpec(handler) {
    return function (renderChildRef) {
      return function (document) {
        var getNode = Halogen_Aff_Driver_State.unRenderStateX(function (v) {
          return v.node;
        });

        var done = function done(st) {
          if (st instanceof Data_Maybe.Just) {
            return Halogen_VDom_Machine.halt(st.value0);
          }

          ;
          return Data_Unit.unit;
        };

        var buildWidget = function buildWidget(spec) {
          var buildThunk = Halogen_VDom_Thunk.buildThunk(Data_Newtype.unwrap(Halogen_HTML_Core.newtypeHTML))(spec);

          var renderComponentSlot = function renderComponentSlot(cs) {
            var v = Effect_Ref.read(renderChildRef)();
            var v1 = v(cs)();
            var node = getNode(v1);
            return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, Data_Maybe.Nothing.value, patch, done));
          };

          var render = function render(slot) {
            if (slot instanceof Halogen_Component.ComponentSlot) {
              return renderComponentSlot(slot.value0);
            }

            ;

            if (slot instanceof Halogen_Component.ThunkSlot) {
              var v = buildThunk(slot.value0);
              return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), new Data_Maybe.Just(v), patch, done));
            }

            ;
            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 85, column 7 - line 90, column 75): " + [slot.constructor.name]);
          };

          var patch = function patch(st, slot) {
            if (st instanceof Data_Maybe.Just) {
              if (slot instanceof Halogen_Component.ComponentSlot) {
                Halogen_VDom_Machine.halt(st.value0);
                return renderComponentSlot(slot.value0);
              }

              ;

              if (slot instanceof Halogen_Component.ThunkSlot) {
                var v = Halogen_VDom_Machine.step(st.value0, slot.value0);
                return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(v), new Data_Maybe.Just(v), patch, done));
              }

              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 98, column 22 - line 104, column 79): " + [slot.constructor.name]);
            }

            ;
            return render(slot);
          };

          return render;
        };

        var buildAttributes = Halogen_VDom_DOM_Prop.buildProp(handler);
        return {
          buildWidget: buildWidget,
          buildAttributes: buildAttributes,
          document: document
        };
      };
    };
  };

  var renderSpec = function renderSpec(document) {
    return function (container) {
      var render = function render(handler) {
        return function (child) {
          return function (v) {
            return function (v1) {
              if (v1 instanceof Data_Maybe.Nothing) {
                return function __do() {
                  var v2 = Effect_Ref["new"](child)();
                  var spec = mkSpec(handler)(v2)(document);
                  var v3 = Halogen_VDom_DOM.buildVDom(spec)(v);
                  var node = Halogen_VDom_Machine.extract(v3);
                  Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(node)(Web_HTML_HTMLElement.toNode(container)))();
                  return {
                    machine: v3,
                    node: node,
                    renderChildRef: v2
                  };
                };
              }

              ;

              if (v1 instanceof Data_Maybe.Just) {
                return function __do() {
                  Effect_Ref.write(child)(v1.value0.renderChildRef)();
                  var v2 = Web_DOM_Node.parentNode(v1.value0.node)();
                  var v3 = Web_DOM_Node.nextSibling(v1.value0.node)();
                  var v4 = Halogen_VDom_Machine.step(v1.value0.machine, v);
                  var newNode = Halogen_VDom_Machine.extract(v4);
                  Control_Applicative.when(Effect.applicativeEffect)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean)))(Unsafe_Reference.unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(v3)(v2))();
                  return {
                    machine: v4,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }

              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 159, column 5 - line 175, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };

      return {
        render: render,
        renderChild: Control_Category.identity(Control_Category.categoryFn),
        removeChild: removeChild,
        dispose: removeChild
      };
    };
  };

  var runUI = function runUI(component) {
    return function (i) {
      return function (element) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor.map(Effect.functorEffect)(Web_HTML_HTMLDocument.toDocument)(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))))(function (v) {
          return Halogen_Aff_Driver.runUI(renderSpec(v)(element))(component)(i);
        });
      };
    };
  };

  exports["runUI"] = runUI;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex.Parser"] = $PS["Routing.Duplex.Parser"] || {};
  var exports = $PS["Routing.Duplex.Parser"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Array = $PS["Data.Array"];
  var Data_Array_NonEmpty = $PS["Data.Array.NonEmpty"];
  var Data_Array_NonEmpty_Internal = $PS["Data.Array.NonEmpty.Internal"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Global_Unsafe = $PS["Global.Unsafe"];

  var Expected = function () {
    function Expected(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Expected.create = function (value0) {
      return function (value1) {
        return new Expected(value0, value1);
      };
    };

    return Expected;
  }();

  var ExpectedEndOfPath = function () {
    function ExpectedEndOfPath(value0) {
      this.value0 = value0;
    }

    ;

    ExpectedEndOfPath.create = function (value0) {
      return new ExpectedEndOfPath(value0);
    };

    return ExpectedEndOfPath;
  }();

  var EndOfPath = function () {
    function EndOfPath() {}

    ;
    EndOfPath.value = new EndOfPath();
    return EndOfPath;
  }();

  var Fail = function () {
    function Fail(value0) {
      this.value0 = value0;
    }

    ;

    Fail.create = function (value0) {
      return new Fail(value0);
    };

    return Fail;
  }();

  var Success = function () {
    function Success(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Success.create = function (value0) {
      return function (value1) {
        return new Success(value0, value1);
      };
    };

    return Success;
  }();

  var Alt = function () {
    function Alt(value0) {
      this.value0 = value0;
    }

    ;

    Alt.create = function (value0) {
      return new Alt(value0);
    };

    return Alt;
  }();

  var Chomp = function () {
    function Chomp(value0) {
      this.value0 = value0;
    }

    ;

    Chomp.create = function (value0) {
      return new Chomp(value0);
    };

    return Chomp;
  }();

  var Prefix = function () {
    function Prefix(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Prefix.create = function (value0) {
      return function (value1) {
        return new Prefix(value0, value1);
      };
    };

    return Prefix;
  }();

  var take = new Chomp(function (state) {
    var v = Data_Array.uncons(state.segments);

    if (v instanceof Data_Maybe.Just) {
      return new Success({
        segments: v.value0.tail,
        params: state.params,
        hash: state.hash
      }, v.value0.head);
    }

    ;
    return new Fail(EndOfPath.value);
  });
  var prefix = Prefix.create;

  var parsePath = function () {
    var toRouteState = function toRouteState(v) {
      return {
        segments: v.value0.value0,
        params: v.value0.value1,
        hash: v.value1
      };
    };

    var splitNonEmpty = function splitNonEmpty(v) {
      return function (v1) {
        if (v1 === "") {
          return [];
        }

        ;
        return Data_String_Common.split(v)(v1);
      };
    };

    var splitSegments = function () {
      var $235 = splitNonEmpty("/");
      return function ($236) {
        return function (v) {
          if (v.length === 2 && v[0] === "" && v[1] === "") {
            return [""];
          }

          ;
          return Data_Functor.map(Data_Functor.functorArray)(Global_Unsafe.unsafeDecodeURIComponent)(v);
        }($235($236));
      };
    }();

    var splitAt = function splitAt(k) {
      return function (p) {
        return function (str) {
          var v = Data_String_CodeUnits.indexOf(p)(str);

          if (v instanceof Data_Maybe.Just) {
            return new Data_Tuple.Tuple(Data_String_CodeUnits.take(v.value0)(str), Data_String_CodeUnits.drop(v.value0 + Data_String_CodeUnits.length(p) | 0)(str));
          }

          ;

          if (v instanceof Data_Maybe.Nothing) {
            return k(str);
          }

          ;
          throw new Error("Failed pattern match at Routing.Duplex.Parser (line 183, column 5 - line 185, column 23): " + [v.constructor.name]);
        };
      };
    };

    var splitKeyValue = function () {
      var $237 = Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(Global_Unsafe.unsafeDecodeURIComponent)(Global_Unsafe.unsafeDecodeURIComponent);
      var $238 = splitAt(Data_Function.flip(Data_Tuple.Tuple.create)(""))("=");
      return function ($239) {
        return $237($238($239));
      };
    }();

    var splitParams = function () {
      var $240 = Data_Functor.map(Data_Functor.functorArray)(splitKeyValue);
      var $241 = splitNonEmpty("&");
      return function ($242) {
        return $240($241($242));
      };
    }();

    var splitPath = function () {
      var $243 = Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(splitSegments)(splitParams);
      var $244 = splitAt(Data_Function.flip(Data_Tuple.Tuple.create)(""))("?");
      return function ($245) {
        return $243($244($245));
      };
    }();

    var $246 = Data_Bifunctor.lmap(Data_Tuple.bifunctorTuple)(splitPath);
    var $247 = splitAt(Data_Function.flip(Data_Tuple.Tuple.create)(""))("#");
    return function ($248) {
      return toRouteState($246($247($248)));
    };
  }();

  var hash = new Chomp(function (state) {
    return new Success(state, state.hash);
  });
  var functorRouteResult = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof Fail) {
        return new Fail(m.value0);
      }

      ;

      if (m instanceof Success) {
        return new Success(m.value0, f(m.value1));
      }

      ;
      throw new Error("Failed pattern match at Routing.Duplex.Parser (line 53, column 1 - line 53, column 58): " + [m.constructor.name]);
    };
  });
  var functorRouteParser = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof Alt) {
        return new Alt(Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray)(Data_Functor.map(functorRouteParser)(f))(m.value0));
      }

      ;

      if (m instanceof Chomp) {
        return new Chomp(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(functorRouteResult)(f))(m.value0));
      }

      ;

      if (m instanceof Prefix) {
        return new Prefix(m.value0, Data_Functor.map(functorRouteParser)(f)(m.value1));
      }

      ;
      throw new Error("Failed pattern match at Routing.Duplex.Parser (line 72, column 1 - line 72, column 58): " + [m.constructor.name]);
    };
  });
  var end = new Chomp(function (state) {
    var v = Data_Array.head(state.segments);

    if (v instanceof Data_Maybe.Nothing) {
      return new Success(state, Data_Unit.unit);
    }

    ;

    if (v instanceof Data_Maybe.Just) {
      return new Fail(new ExpectedEndOfPath(v.value0));
    }

    ;
    throw new Error("Failed pattern match at Routing.Duplex.Parser (line 254, column 3 - line 256, column 45): " + [v.constructor.name]);
  });

  var chompPrefix = function chompPrefix(pre) {
    return function (state) {
      var v = Data_Array.head(state.segments);

      if (v instanceof Data_Maybe.Just && pre === v.value0) {
        return new Success({
          segments: Data_Array.drop(1)(state.segments),
          params: state.params,
          hash: state.hash
        }, Data_Unit.unit);
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return Fail.create(new Expected(pre, v.value0));
      }

      ;
      return Fail.create(EndOfPath.value);
    };
  };

  var runRouteParser = function () {
    var goAlt = function goAlt(v) {
      return function (v1) {
        return function (v2) {
          if (v1 instanceof Fail) {
            return runRouteParser(v)(v2);
          }

          ;
          return v1;
        };
      };
    };

    var go = function go($copy_state) {
      return function ($copy_v) {
        var $tco_var_state = $copy_state;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(state, v) {
          if (v instanceof Alt) {
            $tco_done = true;
            return Data_Foldable.foldl(Data_Array_NonEmpty_Internal.foldableNonEmptyArray)(goAlt(state))(new Fail(EndOfPath.value))(v.value0);
          }

          ;

          if (v instanceof Chomp) {
            $tco_done = true;
            return v.value0(state);
          }

          ;

          if (v instanceof Prefix) {
            var v1 = chompPrefix(v.value0)(state);

            if (v1 instanceof Fail) {
              $tco_done = true;
              return new Fail(v1.value0);
            }

            ;

            if (v1 instanceof Success) {
              $tco_var_state = v1.value0;
              $copy_v = v.value1;
              return;
            }

            ;
            throw new Error("Failed pattern match at Routing.Duplex.Parser (line 149, column 7 - line 151, column 40): " + [v1.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Routing.Duplex.Parser (line 145, column 14 - line 151, column 40): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_state, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go;
  }();

  var run = function run(p) {
    var $251 = Data_Function.flip(runRouteParser)(p);
    return function ($252) {
      return function (v) {
        if (v instanceof Fail) {
          return new Data_Either.Left(v.value0);
        }

        ;

        if (v instanceof Success) {
          return new Data_Either.Right(v.value1);
        }

        ;
        throw new Error("Failed pattern match at Routing.Duplex.Parser (line 188, column 49 - line 190, column 29): " + [v.constructor.name]);
      }($251(parsePath($252)));
    };
  };

  var as = function as(print) {
    return function (decode) {
      return function (p) {
        return new Chomp(function (state) {
          var v = runRouteParser(state)(p);

          if (v instanceof Fail) {
            return new Fail(v.value0);
          }

          ;

          if (v instanceof Success) {
            var v1 = decode(v.value1);

            if (v1 instanceof Data_Either.Left) {
              return Fail.create(new Expected(v1.value0, print(v.value1)));
            }

            ;

            if (v1 instanceof Data_Either.Right) {
              return new Success(v.value0, v1.value0);
            }

            ;
            throw new Error("Failed pattern match at Routing.Duplex.Parser (line 242, column 7 - line 244, column 36): " + [v1.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Routing.Duplex.Parser (line 239, column 3 - line 244, column 36): " + [v.constructor.name]);
        });
      };
    };
  };

  var applyRouteParser = new Control_Apply.Apply(function () {
    return functorRouteParser;
  }, function (fx) {
    return function (x) {
      return new Chomp(function (state) {
        var v = runRouteParser(state)(fx);

        if (v instanceof Fail) {
          return new Fail(v.value0);
        }

        ;

        if (v instanceof Success) {
          return Data_Functor.map(functorRouteResult)(v.value1)(runRouteParser(v.value0)(x));
        }

        ;
        throw new Error("Failed pattern match at Routing.Duplex.Parser (line 76, column 5 - line 78, column 56): " + [v.constructor.name]);
      });
    };
  });
  var applicativeRouteParser = new Control_Applicative.Applicative(function () {
    return applyRouteParser;
  }, function () {
    var $253 = Data_Function.flip(Success.create);
    return function ($254) {
      return Chomp.create($253($254));
    };
  }());

  var altSnoc = function altSnoc(ls) {
    return function (v) {
      var v1 = function v1(v2) {
        return Data_Array_NonEmpty.snoc(ls)(v);
      };

      if (v instanceof Prefix) {
        var $196 = Data_Array_NonEmpty.last(ls);

        if ($196 instanceof Prefix) {
          var $197 = v.value0 === $196.value0;

          if ($197) {
            return Data_Array_NonEmpty["snoc'"](Data_Array_NonEmpty.init(ls))(new Prefix(v.value0, Control_Alt.alt(altRouteParser)($196.value1)(v.value1)));
          }

          ;
          return v1(true);
        }

        ;
        return v1(true);
      }

      ;
      return v1(true);
    };
  };

  var altRouteParser = new Control_Alt.Alt(function () {
    return functorRouteParser;
  }, function (v) {
    return function (v1) {
      if (v instanceof Alt && v1 instanceof Alt) {
        return new Alt(altAppend(v.value0)(v1.value0));
      }

      ;

      if (v instanceof Alt) {
        return new Alt(altSnoc(v.value0)(v1));
      }

      ;

      if (v1 instanceof Alt) {
        return new Alt(altCons(v)(v1.value0));
      }

      ;

      if (v instanceof Prefix && v1 instanceof Prefix && v.value0 === v1.value0) {
        return new Prefix(v.value0, Control_Alt.alt(altRouteParser)(v.value1)(v1.value1));
      }

      ;
      return new Alt(Data_Array_NonEmpty.cons(v)(Data_Array_NonEmpty.singleton(v1)));
    };
  });

  var altCons = function altCons(v) {
    return function (rs) {
      var v1 = function v1(v2) {
        return Data_Array_NonEmpty.cons(v)(rs);
      };

      if (v instanceof Prefix) {
        var $216 = Data_Array_NonEmpty.head(rs);

        if ($216 instanceof Prefix) {
          var $217 = v.value0 === $216.value0;

          if ($217) {
            return Data_Array_NonEmpty["cons'"](new Prefix(v.value0, Control_Alt.alt(altRouteParser)(v.value1)($216.value1)))(Data_Array_NonEmpty.tail(rs));
          }

          ;
          return v1(true);
        }

        ;
        return v1(true);
      }

      ;
      return v1(true);
    };
  };

  var altAppend = function altAppend($copy_ls) {
    return function ($copy_rs) {
      var $tco_var_ls = $copy_ls;
      var $tco_done = false;
      var $tco_result;

      function $tco_loop(ls, rs) {
        var v = function v(v1) {
          if (Data_Boolean.otherwise) {
            return Data_Semigroup.append(Data_Array_NonEmpty_Internal.semigroupNonEmptyArray)(ls)(rs);
          }

          ;
          throw new Error("Failed pattern match at Routing.Duplex.Parser (line 98, column 1 - line 101, column 32): " + [ls.constructor.name, rs.constructor.name]);
        };

        var $226 = Data_Array_NonEmpty.last(ls);

        if ($226 instanceof Prefix) {
          var $227 = Data_Array_NonEmpty.head(rs);

          if ($227 instanceof Prefix) {
            var $228 = $226.value0 === $227.value0;

            if ($228) {
              var rs$prime = Data_Array_NonEmpty["cons'"](new Prefix($226.value0, Control_Alt.alt(altRouteParser)($226.value1)($227.value1)))(Data_Array_NonEmpty.tail(rs));
              var v1 = Data_Array_NonEmpty.fromArray(Data_Array_NonEmpty.init(ls));

              if (v1 instanceof Data_Maybe.Just) {
                $tco_var_ls = v1.value0;
                $copy_rs = rs$prime;
                return;
              }

              ;

              if (v1 instanceof Data_Maybe.Nothing) {
                $tco_done = true;
                return rs$prime;
              }

              ;
              throw new Error("Failed pattern match at Routing.Duplex.Parser (line 110, column 9 - line 112, column 26): " + [v1.constructor.name]);
            }

            ;
            $tco_done = true;
            return v(true);
          }

          ;
          $tco_done = true;
          return v(true);
        }

        ;
        $tco_done = true;
        return v(true);
      }

      ;

      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_ls, $copy_rs);
      }

      ;
      return $tco_result;
    };
  };

  exports["run"] = run;
  exports["prefix"] = prefix;
  exports["take"] = take;
  exports["as"] = as;
  exports["end"] = end;
  exports["functorRouteParser"] = functorRouteParser;
  exports["applyRouteParser"] = applyRouteParser;
  exports["applicativeRouteParser"] = applicativeRouteParser;
  exports["altRouteParser"] = altRouteParser;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex.Types"] = $PS["Routing.Duplex.Types"] || {};
  var exports = $PS["Routing.Duplex.Types"];
  var emptyRouteState = {
    segments: [],
    params: [],
    hash: ""
  };
  exports["emptyRouteState"] = emptyRouteState;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex.Printer"] = $PS["Routing.Duplex.Printer"] || {};
  var exports = $PS["Routing.Duplex.Printer"];
  var Control_Category = $PS["Control.Category"];
  var Data_Array = $PS["Data.Array"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Global_Unsafe = $PS["Global.Unsafe"];
  var Routing_Duplex_Types = $PS["Routing.Duplex.Types"];

  var RoutePrinter = function RoutePrinter(x) {
    return x;
  };

  var semigroupRoutePrinter = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
      return function ($27) {
        return v1(v($27));
      };
    };
  });

  var put = function put(str) {
    return function (state) {
      return {
        segments: Data_Array.snoc(state.segments)(str),
        params: state.params,
        hash: state.hash
      };
    };
  };

  var printPath = function printPath(v) {
    var printSegments = function printSegments(v1) {
      if (v1.length === 1 && v1[0] === "") {
        return "/";
      }

      ;
      return Data_String_Common.joinWith("/")(Data_Functor.map(Data_Functor.functorArray)(Global_Unsafe.unsafeEncodeURIComponent)(v1));
    };

    var printParam = function printParam(key) {
      return function (v1) {
        if (v1 === "") {
          return Global_Unsafe.unsafeEncodeURIComponent(key);
        }

        ;
        return Global_Unsafe.unsafeEncodeURIComponent(key) + ("=" + Global_Unsafe.unsafeEncodeURIComponent(v1));
      };
    };

    var printParams = function printParams(v1) {
      if (v1.length === 0) {
        return "";
      }

      ;
      return "?" + Data_String_Common.joinWith("&")(Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.uncurry(printParam))(v1));
    };

    var printHash = function printHash(v1) {
      if (v1 === "") {
        return "";
      }

      ;
      return "#" + v1;
    };

    return printSegments(v.segments) + (printParams(v.params) + printHash(v.hash));
  };

  var newtypeRoutePrinter = new Data_Newtype.Newtype(function (n) {
    return n;
  }, RoutePrinter);

  var run = function () {
    var $28 = Data_Function.applyFlipped(Routing_Duplex_Types.emptyRouteState);
    var $29 = Data_Newtype.unwrap(newtypeRoutePrinter);
    return function ($30) {
      return printPath($28($29($30)));
    };
  }();

  var monoidRoutePRinter = new Data_Monoid.Monoid(function () {
    return semigroupRoutePrinter;
  }, Control_Category.identity(Control_Category.categoryFn));

  var hash = function hash(h) {
    return function (v) {
      return {
        segments: v.segments,
        params: v.params,
        hash: h
      };
    };
  };

  exports["put"] = put;
  exports["run"] = run;
  exports["semigroupRoutePrinter"] = semigroupRoutePrinter;
  exports["monoidRoutePRinter"] = monoidRoutePRinter;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex"] = $PS["Routing.Duplex"] || {};
  var exports = $PS["Routing.Duplex"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Unit = $PS["Data.Unit"];
  var Routing_Duplex_Parser = $PS["Routing.Duplex.Parser"];
  var Routing_Duplex_Printer = $PS["Routing.Duplex.Printer"];

  var RouteDuplex = function () {
    function RouteDuplex(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    RouteDuplex.create = function (value0) {
      return function (value1) {
        return new RouteDuplex(value0, value1);
      };
    };

    return RouteDuplex;
  }();

  var suffix = function suffix(v) {
    return function (s) {
      return new RouteDuplex(function (a) {
        return Data_Semigroup.append(Routing_Duplex_Printer.semigroupRoutePrinter)(v.value0(a))(Routing_Duplex_Printer.put(s));
      }, Control_Apply.applyFirst(Routing_Duplex_Parser.applyRouteParser)(v.value1)(Routing_Duplex_Parser.prefix(s)(Control_Applicative.pure(Routing_Duplex_Parser.applicativeRouteParser)(Data_Unit.unit))));
    };
  };

  var segment = new RouteDuplex(Routing_Duplex_Printer.put, Routing_Duplex_Parser.take);
  var profunctorRouteDuplex = new Data_Profunctor.Profunctor(function (f) {
    return function (g) {
      return function (v) {
        return new RouteDuplex(function ($100) {
          return v.value0(f($100));
        }, Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(g)(v.value1));
      };
    };
  });

  var print = function print(v) {
    return function ($101) {
      return Routing_Duplex_Printer.run(v.value0($101));
    };
  };

  var prefix = function prefix(s) {
    return function (v) {
      return new RouteDuplex(function (a) {
        return Data_Semigroup.append(Routing_Duplex_Printer.semigroupRoutePrinter)(Routing_Duplex_Printer.put(s))(v.value0(a));
      }, Routing_Duplex_Parser.prefix(s)(v.value1));
    };
  };

  var path = function () {
    var $102 = Data_Function.flip(Data_Foldable.foldr(Data_Foldable.foldableArray)(prefix));
    var $103 = Data_String_Common.split("/");
    return function ($104) {
      return $102($103($104));
    };
  }();

  var root = path("");

  var parse = function parse(v) {
    return Routing_Duplex_Parser.run(v.value1);
  };

  var functorRouteDuplex = new Data_Functor.Functor(function (f) {
    return function (m) {
      return new RouteDuplex(m.value0, Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(f)(m.value1));
    };
  });

  var end = function end(v) {
    return new RouteDuplex(v.value0, Control_Apply.applyFirst(Routing_Duplex_Parser.applyRouteParser)(v.value1)(Routing_Duplex_Parser.end));
  };

  var as = function as(f) {
    return function (g) {
      return function (v) {
        return new RouteDuplex(function ($105) {
          return v.value0(f($105));
        }, Routing_Duplex_Parser.as(Control_Category.identity(Control_Category.categoryFn))(g)(v.value1));
      };
    };
  };

  var applyRouteDuplex = new Control_Apply.Apply(function () {
    return functorRouteDuplex;
  }, function (v) {
    return function (v1) {
      return new RouteDuplex(Control_Apply.apply(Control_Apply.applyFn)(Data_Functor.map(Data_Functor.functorFn)(Data_Semigroup.append(Routing_Duplex_Printer.semigroupRoutePrinter))(v.value0))(v1.value0), Control_Apply.apply(Routing_Duplex_Parser.applyRouteParser)(v.value1)(v1.value1));
    };
  });
  var applicativeRouteDuplex = new Control_Applicative.Applicative(function () {
    return applyRouteDuplex;
  }, function () {
    var $106 = RouteDuplex.create(Data_Function["const"](Data_Monoid.mempty(Routing_Duplex_Printer.monoidRoutePRinter)));
    var $107 = Control_Applicative.pure(Routing_Duplex_Parser.applicativeRouteParser);
    return function ($108) {
      return $106($107($108));
    };
  }());
  exports["RouteDuplex"] = RouteDuplex;
  exports["parse"] = parse;
  exports["print"] = print;
  exports["prefix"] = prefix;
  exports["suffix"] = suffix;
  exports["root"] = root;
  exports["end"] = end;
  exports["segment"] = segment;
  exports["as"] = as;
  exports["applicativeRouteDuplex"] = applicativeRouteDuplex;
  exports["profunctorRouteDuplex"] = profunctorRouteDuplex;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.Event.HashChangeEvent.EventTypes"] = $PS["Web.HTML.Event.HashChangeEvent.EventTypes"] || {};
  var exports = $PS["Web.HTML.Event.HashChangeEvent.EventTypes"];
  var hashchange = "hashchange";
  exports["hashchange"] = hashchange;
})(PS);

(function (exports) {
  "use strict";

  exports.hash = function (location) {
    return function () {
      return location.hash;
    };
  };

  exports.setHash = function (hash) {
    return function (location) {
      return function () {
        location.hash = hash;
      };
    };
  };
})(PS["Web.HTML.Location"] = PS["Web.HTML.Location"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.HTML.Location"] = $PS["Web.HTML.Location"] || {};
  var exports = $PS["Web.HTML.Location"];
  var $foreign = $PS["Web.HTML.Location"];
  exports["hash"] = $foreign.hash;
  exports["setHash"] = $foreign.setHash;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Hash"] = $PS["Routing.Hash"] || {};
  var exports = $PS["Routing.Hash"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Effect = $PS["Effect"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_Event_HashChangeEvent_EventTypes = $PS["Web.HTML.Event.HashChangeEvent.EventTypes"];
  var Web_HTML_Location = $PS["Web.HTML.Location"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];

  var setHash = function setHash(h) {
    return Control_Bind.bind(Effect.bindEffect)(Control_Bind.bind(Effect.bindEffect)(Web_HTML.window)(Web_HTML_Window.location))(Web_HTML_Location.setHash(h));
  };

  var getHash = Control_Bind.bind(Effect.bindEffect)(Control_Bind.bind(Effect.bindEffect)(Web_HTML.window)(Web_HTML_Window.location))(function () {
    var $8 = Data_Functor.map(Effect.functorEffect)(function () {
      var $10 = Data_Maybe.fromMaybe("");
      var $11 = Data_String_CodeUnits.stripPrefix("#");
      return function ($12) {
        return $10($11($12));
      };
    }());
    return function ($9) {
      return $8(Web_HTML_Location.hash($9));
    };
  }());

  var foldHashes = function foldHashes(cb) {
    return function (init) {
      return function __do() {
        var v = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref["new"])(Control_Bind.bindFlipped(Effect.bindEffect)(init)(getHash))();
        var v1 = Data_Functor.map(Effect.functorEffect)(Web_HTML_Window.toEventTarget)(Web_HTML.window)();
        var v2 = Web_Event_EventTarget.eventListener(function (v2) {
          return Control_Bind.bindFlipped(Effect.bindEffect)(Data_Function.flip(Effect_Ref.write)(v))(Control_Bind.join(Effect.bindEffect)(Control_Apply.apply(Effect.applyEffect)(Data_Functor.map(Effect.functorEffect)(cb)(Effect_Ref.read(v)))(getHash)));
        })();
        Web_Event_EventTarget.addEventListener(Web_HTML_Event_HashChangeEvent_EventTypes.hashchange)(v2)(false)(v1)();
        return Web_Event_EventTarget.removeEventListener(Web_HTML_Event_HashChangeEvent_EventTypes.hashchange)(v2)(false)(v1);
      };
    };
  };

  var matchesWith = function matchesWith(dictFoldable) {
    return function (parser) {
      return function (cb) {
        var go = function go(a) {
          var $13 = Data_Maybe.maybe(Control_Applicative.pure(Effect.applicativeEffect)(a))(function (b) {
            return Data_Functor.voidRight(Effect.functorEffect)(new Data_Maybe.Just(b))(cb(a)(b));
          });
          var $14 = Data_Foldable.indexl(dictFoldable)(0);
          return function ($15) {
            return $13($14(parser($15)));
          };
        };

        return foldHashes(go)(go(Data_Maybe.Nothing.value));
      };
    };
  };

  exports["getHash"] = getHash;
  exports["setHash"] = setHash;
  exports["matchesWith"] = matchesWith;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex.Generic"] = $PS["Routing.Duplex.Generic"] || {};
  var exports = $PS["Routing.Duplex.Generic"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Category = $PS["Control.Category"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Generic_Rep = $PS["Data.Generic.Rep"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record = $PS["Record"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Duplex_Parser = $PS["Routing.Duplex.Parser"];

  var GRouteDuplexCtr = function GRouteDuplexCtr(gRouteDuplexCtr) {
    this.gRouteDuplexCtr = gRouteDuplexCtr;
  };

  var GRouteDuplex = function GRouteDuplex(gRouteDuplex) {
    this.gRouteDuplex = gRouteDuplex;
  };

  var noArgs = Control_Applicative.pure(Routing_Duplex.applicativeRouteDuplex)(Data_Generic_Rep.NoArguments.value);
  var gRouteNoArguments = new GRouteDuplexCtr(Control_Category.identity(Control_Category.categoryFn));

  var gRouteDuplexCtr = function gRouteDuplexCtr(dict) {
    return dict.gRouteDuplexCtr;
  };

  var gRouteDuplex = function gRouteDuplex(dict) {
    return dict.gRouteDuplex;
  };

  var gRouteSum = function gRouteSum(dictGRouteDuplex) {
    return function (dictGRouteDuplex1) {
      return new GRouteDuplex(function (r) {
        var v = gRouteDuplex(dictGRouteDuplex)(r);
        var v1 = gRouteDuplex(dictGRouteDuplex1)(r);

        var enc = function enc(v2) {
          if (v2 instanceof Data_Generic_Rep.Inl) {
            return v.value0(v2.value0);
          }

          ;

          if (v2 instanceof Data_Generic_Rep.Inr) {
            return v1.value0(v2.value0);
          }

          ;
          throw new Error("Failed pattern match at Routing.Duplex.Generic (line 32, column 11 - line 34, column 22): " + [v2.constructor.name]);
        };

        var dec = Control_Alt.alt(Routing_Duplex_Parser.altRouteParser)(Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(Data_Generic_Rep.Inl.create)(v.value1))(Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(Data_Generic_Rep.Inr.create)(v1.value1));
        return new Routing_Duplex.RouteDuplex(enc, dec);
      });
    };
  };

  var sum = function sum(dictGeneric) {
    return function (dictGRouteDuplex) {
      var $48 = Data_Profunctor.dimap(Routing_Duplex.profunctorRouteDuplex)(Data_Generic_Rep.from(dictGeneric))(Data_Generic_Rep.to(dictGeneric));
      var $49 = gRouteDuplex(dictGRouteDuplex);
      return function ($50) {
        return $48($49($50));
      };
    };
  };

  var gRouteConstructor = function gRouteConstructor(dictIsSymbol) {
    return function (dictCons) {
      return function (dictGRouteDuplexCtr) {
        return new GRouteDuplex(function (r) {
          var v = Routing_Duplex.end(gRouteDuplexCtr(dictGRouteDuplexCtr)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(r)));

          var enc = function enc(v1) {
            return v.value0(v1);
          };

          var dec = Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(Data_Generic_Rep.Constructor)(v.value1);
          return new Routing_Duplex.RouteDuplex(enc, dec);
        });
      };
    };
  };

  var gRouteArgument = new GRouteDuplexCtr(Control_Category.identity(Control_Category.categoryFn));
  var gRouteAll = new GRouteDuplexCtr(function (v) {
    return new Routing_Duplex.RouteDuplex(function (v1) {
      return v.value0(v1);
    }, Data_Functor.map(Routing_Duplex_Parser.functorRouteParser)(Data_Generic_Rep.Argument)(v.value1));
  });
  exports["gRouteDuplexCtr"] = gRouteDuplexCtr;
  exports["sum"] = sum;
  exports["noArgs"] = noArgs;
  exports["gRouteSum"] = gRouteSum;
  exports["gRouteConstructor"] = gRouteConstructor;
  exports["gRouteNoArguments"] = gRouteNoArguments;
  exports["gRouteArgument"] = gRouteArgument;
  exports["gRouteAll"] = gRouteAll;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Routing.Duplex.Generic.Syntax"] = $PS["Routing.Duplex.Generic.Syntax"] || {};
  var exports = $PS["Routing.Duplex.Generic.Syntax"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Duplex_Generic = $PS["Routing.Duplex.Generic"];

  var GSep = function GSep(gsep) {
    this.gsep = gsep;
  };

  var gsepStringRoute = function gsepStringRoute(dictGRouteDuplexCtr) {
    return new GSep(function (a) {
      var $5 = Routing_Duplex.prefix(a);
      var $6 = Routing_Duplex_Generic.gRouteDuplexCtr(dictGRouteDuplexCtr);
      return function ($7) {
        return $5($6($7));
      };
    });
  };

  var gsepRouteString = function gsepRouteString(dictGRouteDuplexCtr) {
    return new GSep(function () {
      var $8 = Routing_Duplex_Generic.gRouteDuplexCtr(dictGRouteDuplexCtr);
      return function ($9) {
        return Routing_Duplex.suffix($8($9));
      };
    }());
  };

  var gsep = function gsep(dict) {
    return dict.gsep;
  };

  exports["gsep"] = gsep;
  exports["gsepStringRoute"] = gsepStringRoute;
  exports["gsepRouteString"] = gsepRouteString;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Username"] = $PS["Rtsv2App.Data.Username"] || {};
  var exports = $PS["Rtsv2App.Data.Username"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Encode_Class = $PS["Data.Argonaut.Encode.Class"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];

  var toString = function toString(v) {
    return v;
  };

  var parse = function parse(v) {
    if (v === "") {
      return Data_Maybe.Nothing.value;
    }

    ;
    return new Data_Maybe.Just(v);
  };

  var eqUsername = new Data_Eq.Eq(function (x) {
    return function (y) {
      return x === y;
    };
  });
  var encodeJsonUsername = Data_Argonaut_Encode_Class.encodeJsonJString;
  var decodeJsonUsername = Data_Argonaut_Decode_Class.decodeJsonString;
  exports["parse"] = parse;
  exports["toString"] = toString;
  exports["eqUsername"] = eqUsername;
  exports["encodeJsonUsername"] = encodeJsonUsername;
  exports["decodeJsonUsername"] = decodeJsonUsername;
})(PS);

(function ($PS) {
  "use strict";

  $PS["Rtsv2App.Data.Route"] = $PS["Rtsv2App.Data.Route"] || {};
  var exports = $PS["Rtsv2App.Data.Route"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Generic_Rep = $PS["Data.Generic.Rep"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Duplex_Generic = $PS["Routing.Duplex.Generic"];
  var Routing_Duplex_Generic_Syntax = $PS["Routing.Duplex.Generic.Syntax"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];

  var Home = function () {
    function Home() {}

    ;
    Home.value = new Home();
    return Home;
  }();

  var Login = function () {
    function Login() {}

    ;
    Login.value = new Login();
    return Login;
  }();

  var Register = function () {
    function Register() {}

    ;
    Register.value = new Register();
    return Register;
  }();

  var Settings = function () {
    function Settings() {}

    ;
    Settings.value = new Settings();
    return Settings;
  }();

  var Profile = function () {
    function Profile(value0) {
      this.value0 = value0;
    }

    ;

    Profile.create = function (value0) {
      return new Profile(value0);
    };

    return Profile;
  }(); // | This combinator transforms a codec over `String` into one that operatos on the `Username` type.


  var uname = Routing_Duplex.as(Rtsv2App_Data_Username.toString)(function () {
    var $35 = Data_Either.note("Bad username");
    return function ($36) {
      return $35(Rtsv2App_Data_Username.parse($36));
    };
  }());
  var genericRoute = new Data_Generic_Rep.Generic(function (x) {
    if (x instanceof Home) {
      return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
    }

    ;

    if (x instanceof Login) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
    }

    ;

    if (x instanceof Register) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)));
    }

    ;

    if (x instanceof Settings) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))));
    }

    ;

    if (x instanceof Profile) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0))));
    }

    ;
    throw new Error("Failed pattern match at Rtsv2App.Data.Route (line 21, column 1 - line 21, column 48): " + [x.constructor.name]);
  }, function (x) {
    if (x instanceof Data_Generic_Rep.Inl) {
      return Home.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
      return Login.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl) {
      return Register.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl) {
      return Settings.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inr) {
      return new Profile(x.value0.value0.value0.value0);
    }

    ;
    throw new Error("Failed pattern match at Rtsv2App.Data.Route (line 21, column 1 - line 21, column 48): " + [x.constructor.name]);
  });
  var routeCodec = Routing_Duplex.root(Routing_Duplex_Generic.sum(genericRoute)(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Home";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Login";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Register";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Settings";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Profile";
  }))()(Routing_Duplex_Generic.gRouteArgument))))))({
    Home: Routing_Duplex_Generic.noArgs,
    Login: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("login")(Routing_Duplex_Generic.noArgs),
    Register: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("register")(Routing_Duplex_Generic.noArgs),
    Settings: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("settings")(Routing_Duplex_Generic.noArgs),
    Profile: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteAll))("profile")(uname(Routing_Duplex.segment)),
    Favorites: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteArgument))("profile")(Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepRouteString(Routing_Duplex_Generic.gRouteAll))(uname(Routing_Duplex.segment))("favorites"))
  }));
  var eqRoute = new Data_Eq.Eq(function (x) {
    return function (y) {
      if (x instanceof Home && y instanceof Home) {
        return true;
      }

      ;

      if (x instanceof Login && y instanceof Login) {
        return true;
      }

      ;

      if (x instanceof Register && y instanceof Register) {
        return true;
      }

      ;

      if (x instanceof Settings && y instanceof Settings) {
        return true;
      }

      ;

      if (x instanceof Profile && y instanceof Profile) {
        return Data_Eq.eq(Rtsv2App_Data_Username.eqUsername)(x.value0)(y.value0);
      }

      ;
      return false;
    };
  });
  exports["Home"] = Home;
  exports["Login"] = Login;
  exports["Register"] = Register;
  exports["Settings"] = Settings;
  exports["Profile"] = Profile;
  exports["routeCodec"] = routeCodec;
  exports["uname"] = uname;
  exports["eqRoute"] = eqRoute;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Api.Endpoint"] = $PS["Rtsv2App.Api.Endpoint"] || {};
  var exports = $PS["Rtsv2App.Api.Endpoint"];
  var Data_Generic_Rep = $PS["Data.Generic.Rep"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Duplex_Generic = $PS["Routing.Duplex.Generic"];
  var Routing_Duplex_Generic_Syntax = $PS["Routing.Duplex.Generic.Syntax"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];

  var Login = function () {
    function Login() {}

    ;
    Login.value = new Login();
    return Login;
  }();

  var User = function () {
    function User() {}

    ;
    User.value = new User();
    return User;
  }();

  var Users = function () {
    function Users() {}

    ;
    Users.value = new Users();
    return Users;
  }();

  var RoundTripTimes = function () {
    function RoundTripTimes() {}

    ;
    RoundTripTimes.value = new RoundTripTimes();
    return RoundTripTimes;
  }();

  var Profiles = function () {
    function Profiles(value0) {
      this.value0 = value0;
    }

    ;

    Profiles.create = function (value0) {
      return new Profiles(value0);
    };

    return Profiles;
  }();

  var genericEndpoint = new Data_Generic_Rep.Generic(function (x) {
    if (x instanceof Login) {
      return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
    }

    ;

    if (x instanceof User) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
    }

    ;

    if (x instanceof Users) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)));
    }

    ;

    if (x instanceof RoundTripTimes) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))));
    }

    ;

    if (x instanceof Profiles) {
      return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0))));
    }

    ;
    throw new Error("Failed pattern match at Rtsv2App.Api.Endpoint (line 76, column 1 - line 76, column 54): " + [x.constructor.name]);
  }, function (x) {
    if (x instanceof Data_Generic_Rep.Inl) {
      return Login.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
      return User.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl) {
      return Users.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl) {
      return RoundTripTimes.value;
    }

    ;

    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inr) {
      return new Profiles(x.value0.value0.value0.value0);
    }

    ;
    throw new Error("Failed pattern match at Rtsv2App.Api.Endpoint (line 76, column 1 - line 76, column 54): " + [x.constructor.name]);
  });
  var endpointCodec = Routing_Duplex.root(Routing_Duplex.prefix("api")(Routing_Duplex_Generic.sum(genericEndpoint)(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Login";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "User";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Users";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteSum(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "RoundTripTimes";
  }))()(Routing_Duplex_Generic.gRouteNoArguments))(Routing_Duplex_Generic.gRouteConstructor(new Data_Symbol.IsSymbol(function () {
    return "Profiles";
  }))()(Routing_Duplex_Generic.gRouteArgument))))))({
    Login: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("users")(Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("login")(Routing_Duplex_Generic.noArgs)),
    User: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("user")(Routing_Duplex_Generic.noArgs),
    Users: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("users")(Routing_Duplex_Generic.noArgs),
    RoundTripTimes: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteNoArguments))("roundtriptimes")(Routing_Duplex_Generic.noArgs),
    Profiles: Routing_Duplex_Generic_Syntax.gsep(Routing_Duplex_Generic_Syntax.gsepStringRoute(Routing_Duplex_Generic.gRouteAll))("profiles")(Rtsv2App_Data_Route.uname(Routing_Duplex.segment))
  })));
  exports["Login"] = Login;
  exports["User"] = User;
  exports["Users"] = Users;
  exports["Profiles"] = Profiles;
  exports["endpointCodec"] = endpointCodec;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Avatar"] = $PS["Rtsv2App.Data.Avatar"] || {};
  var exports = $PS["Rtsv2App.Data.Avatar"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Encode_Class = $PS["Data.Argonaut.Encode.Class"];
  var Data_Maybe = $PS["Data.Maybe"];

  var toString = function toString(v) {
    return v;
  };

  var parse = function parse(v) {
    if (v === "") {
      return Data_Maybe.Nothing.value;
    }

    ;
    return new Data_Maybe.Just(v);
  };

  var encodeJsonAvatar = Data_Argonaut_Encode_Class.encodeJsonJString;
  var decodeJsonAvatar = Data_Argonaut_Decode_Class.decodeJsonString;
  exports["parse"] = parse;
  exports["toString"] = toString;
  exports["encodeJsonAvatar"] = encodeJsonAvatar;
  exports["decodeJsonAvatar"] = decodeJsonAvatar;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Email"] = $PS["Rtsv2App.Data.Email"] || {};
  var exports = $PS["Rtsv2App.Data.Email"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Encode_Class = $PS["Data.Argonaut.Encode.Class"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Email = function Email(x) {
    return x;
  };

  var newtypeEmail = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Email);
  var encodeJsonEmail = Data_Argonaut_Encode_Class.encodeJsonJString;
  var decodeJsonEmail = Data_Argonaut_Decode_Class.decodeJsonString;
  exports["Email"] = Email;
  exports["newtypeEmail"] = newtypeEmail;
  exports["encodeJsonEmail"] = encodeJsonEmail;
  exports["decodeJsonEmail"] = decodeJsonEmail;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Utils"] = $PS["Rtsv2App.Data.Utils"] || {};
  var exports = $PS["Rtsv2App.Data.Utils"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Tolerant_Combinators = $PS["Data.Argonaut.Decode.Struct.Tolerant.Combinators"];
  var Data_Either = $PS["Data.Either"];

  var decodeAt = function decodeAt(dictDecodeJson) {
    return function (key) {
      return Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(function (v) {
        return Data_Argonaut_Decode_Struct_Tolerant_Combinators.getField(dictDecodeJson)(v)(key);
      })(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeForeignObject(Data_Argonaut_Decode_Class.decodeJsonJson)));
    };
  };

  exports["decodeAt"] = decodeAt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Type.Proxying.Symbol"] = $PS["Type.Proxying.Symbol"] || {};
  var exports = $PS["Type.Proxying.Symbol"];
  var Data_Symbol = $PS["Data.Symbol"];

  var SProxying = function SProxying(sProxy) {
    this.sProxy = sProxy;
  };

  var sProxyingSProxy = new SProxying(Data_Symbol.SProxy.value);
  exports["sProxyingSProxy"] = sProxyingSProxy;
})(PS);

(function (exports) {
  "use strict";

  exports._getItem = function (key) {
    return function (storage) {
      return function () {
        return storage.getItem(key);
      };
    };
  };

  exports.setItem = function (key) {
    return function (value) {
      return function (storage) {
        return function () {
          storage.setItem(key, value);
        };
      };
    };
  };

  exports.removeItem = function (key) {
    return function (storage) {
      return function () {
        storage.removeItem(key);
      };
    };
  };
})(PS["Web.Storage.Storage"] = PS["Web.Storage.Storage"] || {});

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Web.Storage.Storage"] = $PS["Web.Storage.Storage"] || {};
  var exports = $PS["Web.Storage.Storage"];
  var $foreign = $PS["Web.Storage.Storage"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];

  var getItem = function getItem(s) {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $4 = $foreign["_getItem"](s);
    return function ($5) {
      return $3($4($5));
    };
  };

  exports["getItem"] = getItem;
  exports["setItem"] = $foreign.setItem;
  exports["removeItem"] = $foreign.removeItem;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Api.Request"] = $PS["Rtsv2App.Api.Request"] || {};
  var exports = $PS["Rtsv2App.Api.Request"];
  var Affjax = $PS["Affjax"];
  var Affjax_RequestBody = $PS["Affjax.RequestBody"];
  var Affjax_RequestHeader = $PS["Affjax.RequestHeader"];
  var Affjax_ResponseFormat = $PS["Affjax.ResponseFormat"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Tolerant_DecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Argonaut_Encode_Class = $PS["Data.Argonaut.Encode.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HTTP_Method = $PS["Data.HTTP.Method"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Effect = $PS["Effect"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Record_Builder = $PS["Record.Builder"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Rtsv2App_Api_Endpoint = $PS["Rtsv2App.Api.Endpoint"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Email = $PS["Rtsv2App.Data.Email"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];
  var Rtsv2App_Data_Utils = $PS["Rtsv2App.Data.Utils"];
  var Type_Proxying_Symbol = $PS["Type.Proxying.Symbol"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];
  var Web_Storage_Storage = $PS["Web.Storage.Storage"];

  var Token = function Token(x) {
    return x;
  };

  var Get = function () {
    function Get() {}

    ;
    Get.value = new Get();
    return Get;
  }();

  var Post = function () {
    function Post(value0) {
      this.value0 = value0;
    }

    ;

    Post.create = function (value0) {
      return new Post(value0);
    };

    return Post;
  }();

  var Put = function () {
    function Put(value0) {
      this.value0 = value0;
    }

    ;

    Put.create = function (value0) {
      return new Put(value0);
    };

    return Put;
  }();

  var Delete = function () {
    function Delete() {}

    ;
    Delete.value = new Delete();
    return Delete;
  }();

  var tokenKey = "token";

  var writeToken = function writeToken(v) {
    return Control_Bind.bindFlipped(Effect.bindEffect)(Web_Storage_Storage.setItem(tokenKey)(v))(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.localStorage)(Web_HTML.window));
  };

  var removeToken = Control_Bind.bindFlipped(Effect.bindEffect)(Web_Storage_Storage.removeItem(tokenKey))(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.localStorage)(Web_HTML.window));

  var readToken = function __do() {
    var v = Control_Bind.bindFlipped(Effect.bindEffect)(Web_Storage_Storage.getItem(tokenKey))(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.localStorage)(Web_HTML.window))();
    return Data_Functor.map(Data_Maybe.functorMaybe)(Token)(v);
  };

  var defaultRequest = function defaultRequest(v) {
    return function (auth) {
      return function (v1) {
        var v2 = function () {
          if (v1.method instanceof Get) {
            return new Data_Tuple.Tuple(Data_HTTP_Method.GET.value, Data_Maybe.Nothing.value);
          }

          ;

          if (v1.method instanceof Post) {
            return new Data_Tuple.Tuple(Data_HTTP_Method.POST.value, v1.method.value0);
          }

          ;

          if (v1.method instanceof Put) {
            return new Data_Tuple.Tuple(Data_HTTP_Method.PUT.value, v1.method.value0);
          }

          ;

          if (v1.method instanceof Delete) {
            return new Data_Tuple.Tuple(Data_HTTP_Method.DELETE.value, Data_Maybe.Nothing.value);
          }

          ;
          throw new Error("Failed pattern match at Rtsv2App.Api.Request (line 132, column 23 - line 136, column 35): " + [v1.method.constructor.name]);
        }();

        return {
          method: new Data_Either.Left(v2.value0),
          url: v + Routing_Duplex.print(Rtsv2App_Api_Endpoint.endpointCodec)(v1.endpoint),
          headers: function () {
            if (auth instanceof Data_Maybe.Nothing) {
              return [];
            }

            ;

            if (auth instanceof Data_Maybe.Just) {
              return [Affjax_RequestHeader.RequestHeader.create("Authorization")("Token " + auth.value0)];
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Api.Request (line 122, column 14 - line 124, column 74): " + [auth.constructor.name]);
          }(),
          content: Data_Functor.map(Data_Maybe.functorMaybe)(Affjax_RequestBody.json)(v2.value1),
          username: Data_Maybe.Nothing.value,
          password: Data_Maybe.Nothing.value,
          withCredentials: false,
          responseFormat: Affjax_ResponseFormat.json
        };
      };
    };
  };

  var decodeParts = function decodeParts(dictApply) {
    return function (decoder1) {
      return function (decoder2) {
        return function (value) {
          return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Tuple.Tuple.create)(decoder1(value)))(decoder2(value));
        };
      };
    };
  };

  var decodeAuthProfile = decodeParts(Data_Either.applyEither)(function () {
    var $40 = Data_Functor.map(Data_Either.functorEither)(Token);
    var $41 = Rtsv2App_Data_Utils.decodeAt(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeDecodeJson(Data_Argonaut_Decode_Class.decodeJsonString))("token");
    return function ($42) {
      return $40($41($42));
    };
  }())(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeJson(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeJsonRecord(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Data_Argonaut_Decode_Class.decodeJsonString))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Rtsv2App_Data_Avatar.decodeJsonAvatar))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_nonPlus()(Rtsv2App_Data_Username.decodeJsonUsername)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_NilNilNil(Record_Builder.categoryBuilder)(Data_Operator_Top.top1_Either))(new Data_Symbol.IsSymbol(function () {
    return "username";
  }))()(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "username";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
    return "image";
  }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "image";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
    return "bio";
  }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "bio";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))()));

  var requestUser = function requestUser(dictMonadAff) {
    return function (baseUrl) {
      return function (opts) {
        return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Effect_Aff_Class.liftAff(dictMonadAff)(Affjax.request(defaultRequest(baseUrl)(Data_Maybe.Nothing.value)(opts))))(function (v) {
          return Control_Applicative.pure(dictMonadAff.MonadEffect0().Monad0().Applicative0())(Control_Bind.bindFlipped(Data_Either.bindEither)(decodeAuthProfile)(Control_Bind.bindFlipped(Data_Either.bindEither)(Rtsv2App_Data_Utils.decodeAt(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeDecodeJson(Data_Argonaut_Decode_Class.decodeJsonJson))("user"))(Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Affjax_ResponseFormat.printResponseFormatError)(v.body))));
        });
      };
    };
  };

  var login = function login(dictMonadAff) {
    return function (baseUrl) {
      return function (fields) {
        var method = Post.create(Data_Maybe.Just.create(Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeRecord(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeRecord(Data_Argonaut_Encode_Class.gEncodeJsonCons(Rtsv2App_Data_Email.encodeJsonEmail)(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeJsonJString)(Data_Argonaut_Encode_Class.gEncodeJsonNil)(new Data_Symbol.IsSymbol(function () {
          return "password";
        }))())(new Data_Symbol.IsSymbol(function () {
          return "email";
        }))())())(Data_Argonaut_Encode_Class.gEncodeJsonNil)(new Data_Symbol.IsSymbol(function () {
          return "user";
        }))())())({
          user: fields
        })));
        return requestUser(dictMonadAff)(baseUrl)({
          endpoint: Rtsv2App_Api_Endpoint.Login.value,
          method: method
        });
      };
    };
  };

  var register = function register(dictMonadAff) {
    return function (baseUrl) {
      return function (fields) {
        var method = Post.create(Data_Maybe.Just.create(Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeRecord(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeRecord(Data_Argonaut_Encode_Class.gEncodeJsonCons(Rtsv2App_Data_Email.encodeJsonEmail)(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeJsonJString)(Data_Argonaut_Encode_Class.gEncodeJsonCons(Rtsv2App_Data_Username.encodeJsonUsername)(Data_Argonaut_Encode_Class.gEncodeJsonNil)(new Data_Symbol.IsSymbol(function () {
          return "username";
        }))())(new Data_Symbol.IsSymbol(function () {
          return "password";
        }))())(new Data_Symbol.IsSymbol(function () {
          return "email";
        }))())())(Data_Argonaut_Encode_Class.gEncodeJsonNil)(new Data_Symbol.IsSymbol(function () {
          return "user";
        }))())())({
          user: fields
        })));
        return requestUser(dictMonadAff)(baseUrl)({
          endpoint: Rtsv2App_Api_Endpoint.Users.value,
          method: method
        });
      };
    };
  };

  exports["Get"] = Get;
  exports["Put"] = Put;
  exports["defaultRequest"] = defaultRequest;
  exports["login"] = login;
  exports["register"] = register;
  exports["readToken"] = readToken;
  exports["writeToken"] = writeToken;
  exports["removeToken"] = removeToken;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Capability.Now"] = $PS["Rtsv2App.Capability.Now"] || {};
  var exports = $PS["Rtsv2App.Capability.Now"];

  var Now = function Now(Monad0, now, nowDate, nowDateTime, nowTime) {
    this.Monad0 = Monad0;
    this.now = now;
    this.nowDate = nowDate;
    this.nowDateTime = nowDateTime;
    this.nowTime = nowTime;
  };

  var nowDateTime = function nowDateTime(dict) {
    return dict.nowDateTime;
  };

  exports["nowDateTime"] = nowDateTime;
  exports["Now"] = Now;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Log"] = $PS["Rtsv2App.Data.Log"] || {};
  var exports = $PS["Rtsv2App.Data.Log"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Either = $PS["Data.Either"];
  var Data_Formatter_DateTime = $PS["Data.Formatter.DateTime"];
  var Data_Function = $PS["Data.Function"];
  var Rtsv2App_Capability_Now = $PS["Rtsv2App.Capability.Now"];

  var Debug = function () {
    function Debug() {}

    ;
    Debug.value = new Debug();
    return Debug;
  }();

  var Info = function () {
    function Info() {}

    ;
    Info.value = new Info();
    return Info;
  }();

  var Warn = function () {
    function Warn() {}

    ;
    Warn.value = new Warn();
    return Warn;
  }();

  var $$Error = function () {
    function $$Error() {}

    ;
    $$Error.value = new $$Error();
    return $$Error;
  }();

  var reason = function reason(v) {
    return v.reason;
  };

  var mkLog = function mkLog(dictNow) {
    return function (logReason) {
      return function (inputMessage) {
        var formatTimestamp = function () {
          var $43 = Data_Either.either(Data_Function["const"]("(Failed to assign time)"))(Control_Category.identity(Control_Category.categoryFn));
          var $44 = Data_Formatter_DateTime.formatDateTime("YYYY-DD-MM hh:mm:ss a");
          return function ($45) {
            return $43($44($45));
          };
        }();

        return Control_Bind.bind(dictNow.Monad0().Bind1())(Rtsv2App_Capability_Now.nowDateTime(dictNow))(function (v) {
          var headerWith = function headerWith(start) {
            return "[" + (start + (": " + (formatTimestamp(v) + ("]\x0a" + inputMessage))));
          };

          var formattedLog = function () {
            if (logReason instanceof Debug) {
              return headerWith("DEBUG");
            }

            ;

            if (logReason instanceof Info) {
              return headerWith("INFO");
            }

            ;

            if (logReason instanceof Warn) {
              return headerWith("WARNING");
            }

            ;

            if (logReason instanceof $$Error) {
              return headerWith("ERROR");
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Data.Log (line 95, column 20 - line 99, column 34): " + [logReason.constructor.name]);
          }();

          return Control_Applicative.pure(dictNow.Monad0().Applicative0())({
            reason: logReason,
            timestamp: v,
            message: formattedLog
          });
        });
      };
    };
  };

  var message = function message(v) {
    return v.message;
  };

  exports["Debug"] = Debug;
  exports["Error"] = $$Error;
  exports["message"] = message;
  exports["reason"] = reason;
  exports["mkLog"] = mkLog;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Capability.LogMessages"] = $PS["Rtsv2App.Capability.LogMessages"] || {};
  var exports = $PS["Rtsv2App.Capability.LogMessages"];
  var Control_Bind = $PS["Control.Bind"];
  var Rtsv2App_Data_Log = $PS["Rtsv2App.Data.Log"];

  var LogMessages = function LogMessages(Monad0, logMessage) {
    this.Monad0 = Monad0;
    this.logMessage = logMessage;
  };

  var logMessage = function logMessage(dict) {
    return dict.logMessage;
  };

  var log = function log(dictLogMessages) {
    return function (dictNow) {
      return function (reason) {
        return Control_Bind.composeKleisliFlipped(dictLogMessages.Monad0().Bind1())(logMessage(dictLogMessages))(Rtsv2App_Data_Log.mkLog(dictNow)(reason));
      };
    };
  };

  var logError = function logError(dictLogMessages) {
    return function (dictNow) {
      return log(dictLogMessages)(dictNow)(Rtsv2App_Data_Log["Error"].value);
    };
  };

  exports["LogMessages"] = LogMessages;
  exports["logError"] = logError;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Api.Utils"] = $PS["Rtsv2App.Api.Utils"] || {};
  var exports = $PS["Rtsv2App.Api.Utils"];
  var Affjax = $PS["Affjax"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect_Aff_Bus = $PS["Effect.Aff.Bus"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Rtsv2App_Api_Request = $PS["Rtsv2App.Api.Request"];
  var Rtsv2App_Capability_LogMessages = $PS["Rtsv2App.Capability.LogMessages"];

  var mkRequest = function mkRequest(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (opts) {
        return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Control_Monad_Reader_Class.ask(dictMonadAsk))(function (v) {
          return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Effect_Aff_Class.liftAff(dictMonadAff)(Affjax.request(Rtsv2App_Api_Request.defaultRequest(v.baseUrl)(Data_Maybe.Nothing.value)(opts))))(function (v1) {
            return Control_Applicative.pure(dictMonadAff.MonadEffect0().Monad0().Applicative0())(Data_Either.hush(v1.body));
          });
        });
      };
    };
  };

  var mkAuthRequest = function mkAuthRequest(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (opts) {
        return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Control_Monad_Reader_Class.ask(dictMonadAsk))(function (v) {
          return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Effect_Class.liftEffect(dictMonadAff.MonadEffect0())(Rtsv2App_Api_Request.readToken))(function (v1) {
            return Control_Bind.bind(dictMonadAff.MonadEffect0().Monad0().Bind1())(Effect_Aff_Class.liftAff(dictMonadAff)(Affjax.request(Rtsv2App_Api_Request.defaultRequest(v.baseUrl)(v1)(opts))))(function (v2) {
              return Control_Applicative.pure(dictMonadAff.MonadEffect0().Monad0().Applicative0())(Data_Either.hush(v2.body));
            });
          });
        });
      };
    };
  };

  var decode = function decode(dictLogMessages) {
    return function (dictNow) {
      return function (v) {
        return function (v1) {
          if (v1 instanceof Data_Maybe.Nothing) {
            return Control_Apply.applySecond(dictLogMessages.Monad0().Bind1().Apply0())(Rtsv2App_Capability_LogMessages.logError(dictLogMessages)(dictNow)("Response malformed"))(Control_Applicative.pure(dictLogMessages.Monad0().Applicative0())(Data_Maybe.Nothing.value));
          }

          ;

          if (v1 instanceof Data_Maybe.Just) {
            var v2 = v(v1.value0);

            if (v2 instanceof Data_Either.Left) {
              return Control_Apply.applySecond(dictLogMessages.Monad0().Bind1().Apply0())(Rtsv2App_Capability_LogMessages.logError(dictLogMessages)(dictNow)(v2.value0))(Control_Applicative.pure(dictLogMessages.Monad0().Applicative0())(Data_Maybe.Nothing.value));
            }

            ;

            if (v2 instanceof Data_Either.Right) {
              return Control_Applicative.pure(dictLogMessages.Monad0().Applicative0())(new Data_Maybe.Just(v2.value0));
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Api.Utils (line 84, column 30 - line 86, column 41): " + [v2.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Rtsv2App.Api.Utils (line 82, column 1 - line 82, column 103): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    };
  };

  var decodeWithUser = function decodeWithUser(dictMonadEffect) {
    return function (dictMonadAsk) {
      return function (dictLogMessages) {
        return function (dictNow) {
          return function (decoder) {
            return function (json) {
              return Control_Bind.bind(dictLogMessages.Monad0().Bind1())(Control_Bind.bindFlipped(dictLogMessages.Monad0().Bind1())(function () {
                var $49 = Effect_Class.liftEffect(dictMonadEffect);
                return function ($50) {
                  return $49(Effect_Ref.read($50));
                };
              }())(Control_Monad_Reader_Class.asks(dictMonadAsk)(function (v) {
                return v.userEnv.currentUser;
              })))(function (v) {
                return decode(dictLogMessages)(dictNow)(decoder(Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
                  return v1.username;
                })(v)))(json);
              });
            };
          };
        };
      };
    };
  };

  var authenticate = function authenticate(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (dictLogMessages) {
        return function (dictNow) {
          return function (req) {
            return function (fields) {
              return Control_Bind.bind(dictLogMessages.Monad0().Bind1())(Control_Monad_Reader_Class.ask(dictMonadAsk))(function (v) {
                return Control_Bind.bind(dictLogMessages.Monad0().Bind1())(req(v.baseUrl)(fields))(function (v1) {
                  if (v1 instanceof Data_Either.Left) {
                    return Control_Apply.applySecond(dictLogMessages.Monad0().Bind1().Apply0())(Rtsv2App_Capability_LogMessages.logError(dictLogMessages)(dictNow)(v1.value0))(Control_Applicative.pure(dictLogMessages.Monad0().Applicative0())(Data_Maybe.Nothing.value));
                  }

                  ;

                  if (v1 instanceof Data_Either.Right) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(dictLogMessages.Monad0().Bind1())(Effect_Class.liftEffect(dictMonadAff.MonadEffect0())(function __do() {
                      Rtsv2App_Api_Request.writeToken(v1.value0.value0)();
                      return Effect_Ref.write(new Data_Maybe.Just(v1.value0.value1))(v.userEnv.currentUser)();
                    }))(function () {
                      return Control_Bind.discard(Control_Bind.discardUnit)(dictLogMessages.Monad0().Bind1())(Effect_Aff_Class.liftAff(dictMonadAff)(Effect_Aff_Bus.write(new Data_Maybe.Just(v1.value0.value1))(v.userEnv.userBus)))(function () {
                        return Control_Applicative.pure(dictLogMessages.Monad0().Applicative0())(new Data_Maybe.Just(v1.value0.value1));
                      });
                    });
                  }

                  ;
                  throw new Error("Failed pattern match at Rtsv2App.Api.Utils (line 69, column 26 - line 77, column 26): " + [v1.constructor.name]);
                });
              });
            };
          };
        };
      };
    };
  };

  exports["mkRequest"] = mkRequest;
  exports["mkAuthRequest"] = mkAuthRequest;
  exports["authenticate"] = authenticate;
  exports["decode"] = decode;
  exports["decodeWithUser"] = decodeWithUser;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Capability.Navigate"] = $PS["Rtsv2App.Capability.Navigate"] || {};
  var exports = $PS["Rtsv2App.Capability.Navigate"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];

  var Navigate = function Navigate(Monad0, logout, navigate) {
    this.Monad0 = Monad0;
    this.logout = logout;
    this.navigate = navigate;
  };

  var navigate = function navigate(dict) {
    return dict.navigate;
  };

  var logout = function logout(dict) {
    return dict.logout;
  };

  var navigateHalogenM = function navigateHalogenM(dictNavigate) {
    return new Navigate(function () {
      return Halogen_Query_HalogenM.monadHalogenM;
    }, Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictNavigate.Monad0())(logout(dictNavigate)), function () {
      var $1 = Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictNavigate.Monad0());
      var $2 = navigate(dictNavigate);
      return function ($3) {
        return $1($2($3));
      };
    }());
  };

  exports["logout"] = logout;
  exports["navigate"] = navigate;
  exports["Navigate"] = Navigate;
  exports["navigateHalogenM"] = navigateHalogenM;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Capability.Resource.User"] = $PS["Rtsv2App.Capability.Resource.User"] || {};
  var exports = $PS["Rtsv2App.Capability.Resource.User"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];

  var ManageUser = function ManageUser(Monad0, getAuthor, getCurrentUser, loginUser, registerUser, updateUser) {
    this.Monad0 = Monad0;
    this.getAuthor = getAuthor;
    this.getCurrentUser = getCurrentUser;
    this.loginUser = loginUser;
    this.registerUser = registerUser;
    this.updateUser = updateUser;
  };

  var updateUser = function updateUser(dict) {
    return dict.updateUser;
  };

  var registerUser = function registerUser(dict) {
    return dict.registerUser;
  };

  var loginUser = function loginUser(dict) {
    return dict.loginUser;
  };

  var getCurrentUser = function getCurrentUser(dict) {
    return dict.getCurrentUser;
  };

  var getAuthor = function getAuthor(dict) {
    return dict.getAuthor;
  };

  var manageUserHalogenM = function manageUserHalogenM(dictManageUser) {
    return new ManageUser(function () {
      return Halogen_Query_HalogenM.monadHalogenM;
    }, function () {
      var $1 = Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictManageUser.Monad0());
      var $2 = getAuthor(dictManageUser);
      return function ($3) {
        return $1($2($3));
      };
    }(), Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictManageUser.Monad0())(getCurrentUser(dictManageUser)), function () {
      var $4 = Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictManageUser.Monad0());
      var $5 = loginUser(dictManageUser);
      return function ($6) {
        return $4($5($6));
      };
    }(), function () {
      var $7 = Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictManageUser.Monad0());
      var $8 = registerUser(dictManageUser);
      return function ($9) {
        return $7($8($9));
      };
    }(), function () {
      var $10 = Control_Monad_Trans_Class.lift(Halogen_Query_HalogenM.monadTransHalogenM)(dictManageUser.Monad0());
      var $11 = updateUser(dictManageUser);
      return function ($12) {
        return $10($11($12));
      };
    }());
  };

  exports["getAuthor"] = getAuthor;
  exports["getCurrentUser"] = getCurrentUser;
  exports["loginUser"] = loginUser;
  exports["registerUser"] = registerUser;
  exports["updateUser"] = updateUser;
  exports["ManageUser"] = ManageUser;
  exports["manageUserHalogenM"] = manageUserHalogenM;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Data.Profile"] = $PS["Rtsv2App.Data.Profile"] || {};
  var exports = $PS["Rtsv2App.Data.Profile"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Cross_DecodeJsonWith = $PS["Data.Argonaut.Decode.Struct.Cross.DecodeJsonWith"];
  var Data_Argonaut_Decode_Struct_Tolerant_Cross_Utils = $PS["Data.Argonaut.Decode.Struct.Tolerant.Cross.Utils"];
  var Data_Argonaut_Decode_Struct_Tolerant_DecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Bottom = $PS["Data.Operator.Bottom"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Get_RGet = $PS["Data.Struct.Get.RGet"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Struct_RenameMany_RRenameMany = $PS["Data.Struct.RenameMany.RRenameMany"];
  var Data_Struct_Utils_ReifyKeyAndValueSymbols = $PS["Data.Struct.Utils.ReifyKeyAndValueSymbols"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Builder = $PS["Record.Builder"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];
  var Rtsv2App_Data_Utils = $PS["Rtsv2App.Data.Utils"];
  var Type_Equality = $PS["Type.Equality"];
  var Type_Proxying_Symbol = $PS["Type.Proxying.Symbol"];

  var Following = function () {
    function Following() {}

    ;
    Following.value = new Following();
    return Following;
  }();

  var NotFollowing = function () {
    function NotFollowing() {}

    ;
    NotFollowing.value = new NotFollowing();
    return NotFollowing;
  }();

  var You = function () {
    function You() {}

    ;
    You.value = new You();
    return You;
  }();

  var decodeJsonFollowStatus = new Data_Argonaut_Decode_Class.DecodeJson(Control_Bind.composeKleisli(Data_Either.bindEither)(Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeJsonBoolean))(function (v) {
    if (v) {
      return Control_Applicative.pure(Data_Either.applicativeEither)(Following.value);
    }

    ;
    return Control_Applicative.pure(Data_Either.applicativeEither)(NotFollowing.value);
  }));

  var getRelation = function getRelation(mbUsername) {
    return function (json) {
      return function (record) {
        if (mbUsername instanceof Data_Maybe.Just && Data_Eq.eq(Rtsv2App_Data_Username.eqUsername)(mbUsername.value0)(record.username)) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(You.value);
        }

        ;
        return Data_Argonaut_Decode_Class.decodeJson(decodeJsonFollowStatus)(json);
      };
    };
  };

  var decodeAuthor = function decodeAuthor(mbUsername) {
    var $17 = Data_Functor.map(Data_Either.functorEither)(Data_Struct_RenameMany_RRenameMany.rrenameMany(Data_Struct_RenameMany_RRenameMany.rrenameManyFunction()(Data_Struct_Utils_ReifyKeyAndValueSymbols.reifyKeyAndValueSymbolsCons(new Data_Symbol.IsSymbol(function () {
      return "following";
    }))(new Data_Symbol.IsSymbol(function () {
      return "relation";
    }))(Data_Struct_Utils_ReifyKeyAndValueSymbols.reifyKeyAndValueSymbolsNil))()())()({
      following: Data_Symbol.SProxy.value
    }));
    var $18 = Data_Argonaut_Decode_Struct_Tolerant_Cross_Utils.decodeJsonWith(Data_Either.bindEither)(Data_Operator_Bottom.bottom2Either)(Data_Argonaut_Decode_Struct_Cross_DecodeJsonWith.decodeJsonWithCons(Data_Either.bindEither)(Data_Operator_Bottom.bottom2Either)()()(Data_Argonaut_Decode_Struct_Cross_DecodeJsonWith.decodeJsonWithNil(Record_Builder.categoryBuilder)(Data_Operator_Top.top1_Either))(new Data_Symbol.IsSymbol(function () {
      return "following";
    }))()(Data_Struct_Get_RGet.rgetRecord(new Data_Symbol.IsSymbol(function () {
      return "following";
    }))(Type_Proxying_Symbol.sProxyingSProxy))(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
      return "following";
    }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder)(Data_Operator_Top.top1_Either)(Type_Equality.refl))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Data_Argonaut_Decode_Class.decodeJsonString))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Rtsv2App_Data_Avatar.decodeJsonAvatar))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_nonPlus()(Rtsv2App_Data_Username.decodeJsonUsername)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_NilNilNil(Record_Builder.categoryBuilder)(Data_Operator_Top.top1_Either))(new Data_Symbol.IsSymbol(function () {
      return "username";
    }))()(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
      return "username";
    }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
      return "image";
    }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
      return "image";
    }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
      return "bio";
    }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
      return "bio";
    }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))()()(Data_Operator_Top.top1_Either)({
      following: getRelation(mbUsername)
    });
    return function ($19) {
      return $17($18($19));
    };
  };

  var decodeProfileAuthor = function decodeProfileAuthor(u) {
    return Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(decodeAuthor(u))(Rtsv2App_Data_Utils.decodeAt(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeDecodeJson(Data_Argonaut_Decode_Class.decodeJsonJson))("profile"));
  };

  exports["decodeProfileAuthor"] = decodeProfileAuthor;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Env"] = $PS["Rtsv2App.Env"] || {};
  var exports = $PS["Rtsv2App.Env"];

  var Dev = function () {
    function Dev() {}

    ;
    Dev.value = new Dev();
    return Dev;
  }();

  var Prod = function () {
    function Prod() {}

    ;
    Prod.value = new Prod();
    return Prod;
  }();

  exports["Dev"] = Dev;
  exports["Prod"] = Prod;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.AppM"] = $PS["Rtsv2App.AppM"] || {};
  var exports = $PS["Rtsv2App.AppM"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_Reader_Trans = $PS["Control.Monad.Reader.Trans"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Tolerant_DecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Argonaut_Encode_Class = $PS["Data.Argonaut.Encode.Class"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_Bus = $PS["Effect.Aff.Bus"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Console = $PS["Effect.Console"];
  var Effect_Now = $PS["Effect.Now"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Record_Builder = $PS["Record.Builder"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Hash = $PS["Routing.Hash"];
  var Rtsv2App_Api_Endpoint = $PS["Rtsv2App.Api.Endpoint"];
  var Rtsv2App_Api_Request = $PS["Rtsv2App.Api.Request"];
  var Rtsv2App_Api_Utils = $PS["Rtsv2App.Api.Utils"];
  var Rtsv2App_Capability_LogMessages = $PS["Rtsv2App.Capability.LogMessages"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Capability_Now = $PS["Rtsv2App.Capability.Now"];
  var Rtsv2App_Capability_Resource_User = $PS["Rtsv2App.Capability.Resource.User"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Email = $PS["Rtsv2App.Data.Email"];
  var Rtsv2App_Data_Log = $PS["Rtsv2App.Data.Log"];
  var Rtsv2App_Data_Profile = $PS["Rtsv2App.Data.Profile"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];
  var Rtsv2App_Data_Utils = $PS["Rtsv2App.Data.Utils"];
  var Rtsv2App_Env = $PS["Rtsv2App.Env"];
  var Type_Equality = $PS["Type.Equality"];
  var Type_Proxying_Symbol = $PS["Type.Proxying.Symbol"];

  var AppM = function AppM(x) {
    return x;
  };

  var runAppM = function runAppM(env) {
    return function (v) {
      return Control_Monad_Reader_Trans.runReaderT(v)(env);
    };
  };

  var monadEffectAppM = Control_Monad_Reader_Trans.monadEffectReader(Effect_Aff.monadEffectAff);
  var monadAppM = Control_Monad_Reader_Trans.monadReaderT(Effect_Aff.monadAff);

  var monadAskAppM = function monadAskAppM(dictTypeEquals) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
      return monadAppM;
    }, AppM(Control_Monad_Reader_Class.asks(Control_Monad_Reader_Trans.monadAskReaderT(Effect_Aff.monadAff))(Type_Equality.from(dictTypeEquals))));
  };

  var nowAppM = new Rtsv2App_Capability_Now.Now(function () {
    return monadAppM;
  }, Effect_Class.liftEffect(monadEffectAppM)(Effect_Now.now), Effect_Class.liftEffect(monadEffectAppM)(Effect_Now.nowDate), Effect_Class.liftEffect(monadEffectAppM)(Effect_Now.nowDateTime), Effect_Class.liftEffect(monadEffectAppM)(Effect_Now.nowTime));
  var monadAffAppM = Effect_Aff_Class.monadAffReader(Effect_Aff_Class.monadAffAff);
  var functorAppM = Control_Monad_Reader_Trans.functorReaderT(Effect_Aff.functorAff);
  var bindAppM = Control_Monad_Reader_Trans.bindReaderT(Effect_Aff.bindAff);
  var logMessagesAppM = new Rtsv2App_Capability_LogMessages.LogMessages(function () {
    return monadAppM;
  }, function (log) {
    return Control_Bind.bind(bindAppM)(Control_Monad_Reader_Class.ask(monadAskAppM(Type_Equality.refl)))(function (v) {
      return Effect_Class.liftEffect(monadEffectAppM)(function () {
        var v1 = Rtsv2App_Data_Log.reason(log);

        if (v.logLevel instanceof Rtsv2App_Env.Prod && v1 instanceof Rtsv2App_Data_Log.Debug) {
          return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
        }

        ;
        return Effect_Console.log(Rtsv2App_Data_Log.message(log));
      }());
    });
  });
  var manageUserAppM = new Rtsv2App_Capability_Resource_User.ManageUser(function () {
    return monadAppM;
  }, function (username) {
    return Control_Bind.bind(bindAppM)(Rtsv2App_Api_Utils.mkRequest(monadAffAppM)(monadAskAppM(Type_Equality.refl))({
      endpoint: new Rtsv2App_Api_Endpoint.Profiles(username),
      method: Rtsv2App_Api_Request.Get.value
    }))(Rtsv2App_Api_Utils.decodeWithUser(monadEffectAppM)(monadAskAppM(Type_Equality.refl))(logMessagesAppM)(nowAppM)(Rtsv2App_Data_Profile.decodeProfileAuthor));
  }, Control_Bind.bind(bindAppM)(Rtsv2App_Api_Utils.mkAuthRequest(monadAffAppM)(monadAskAppM(Type_Equality.refl))({
    endpoint: Rtsv2App_Api_Endpoint.User.value,
    method: Rtsv2App_Api_Request.Get.value
  }))(Rtsv2App_Api_Utils.decode(logMessagesAppM)(nowAppM)(Rtsv2App_Data_Utils.decodeAt(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeJsonRecord(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Data_Argonaut_Decode_Class.decodeJsonString))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_nonPlus()(Rtsv2App_Data_Email.decodeJsonEmail)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Rtsv2App_Data_Avatar.decodeJsonAvatar))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_nonPlus()(Rtsv2App_Data_Username.decodeJsonUsername)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_NilNilNil(Record_Builder.categoryBuilder)(Data_Operator_Top.top1_Either))(new Data_Symbol.IsSymbol(function () {
    return "username";
  }))()(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "username";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
    return "image";
  }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "image";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
    return "email";
  }))()(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "email";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
    return "bio";
  }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
    return "bio";
  }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))())("user"))), Rtsv2App_Api_Utils.authenticate(monadAffAppM)(monadAskAppM(Type_Equality.refl))(logMessagesAppM)(nowAppM)(Rtsv2App_Api_Request.login(monadAffAppM)), Rtsv2App_Api_Utils.authenticate(monadAffAppM)(monadAskAppM(Type_Equality.refl))(logMessagesAppM)(nowAppM)(Rtsv2App_Api_Request.register(monadAffAppM)), function (fields) {
    return Data_Functor["void"](functorAppM)(Rtsv2App_Api_Utils.mkAuthRequest(monadAffAppM)(monadAskAppM(Type_Equality.refl))({
      endpoint: Rtsv2App_Api_Endpoint.User.value,
      method: new Rtsv2App_Api_Request.Put(new Data_Maybe.Just(Data_Argonaut_Encode_Class.encodeJson(Data_Argonaut_Encode_Class.encodeRecord(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeJsonMaybe(Data_Argonaut_Encode_Class.encodeJsonJString))(Data_Argonaut_Encode_Class.gEncodeJsonCons(Rtsv2App_Data_Email.encodeJsonEmail)(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeJsonMaybe(Rtsv2App_Data_Avatar.encodeJsonAvatar))(Data_Argonaut_Encode_Class.gEncodeJsonCons(Data_Argonaut_Encode_Class.encodeJsonMaybe(Data_Argonaut_Encode_Class.encodeJsonJString))(Data_Argonaut_Encode_Class.gEncodeJsonCons(Rtsv2App_Data_Username.encodeJsonUsername)(Data_Argonaut_Encode_Class.gEncodeJsonNil)(new Data_Symbol.IsSymbol(function () {
        return "username";
      }))())(new Data_Symbol.IsSymbol(function () {
        return "password";
      }))())(new Data_Symbol.IsSymbol(function () {
        return "image";
      }))())(new Data_Symbol.IsSymbol(function () {
        return "email";
      }))())(new Data_Symbol.IsSymbol(function () {
        return "bio";
      }))())())(fields)))
    }));
  });
  var navigateAppM = new Rtsv2App_Capability_Navigate.Navigate(function () {
    return monadAppM;
  }, Control_Bind.bind(bindAppM)(Control_Monad_Reader_Class.asks(monadAskAppM(Type_Equality.refl))(function (v) {
    return v.userEnv;
  }))(function (v) {
    return Control_Bind.discard(Control_Bind.discardUnit)(bindAppM)(Effect_Class.liftEffect(monadEffectAppM)(function __do() {
      Effect_Ref.write(Data_Maybe.Nothing.value)(v.currentUser)();
      return Rtsv2App_Api_Request.removeToken();
    }))(function () {
      return Control_Bind.discard(Control_Bind.discardUnit)(bindAppM)(Effect_Aff_Class.liftAff(monadAffAppM)(Effect_Aff_Bus.write(Data_Maybe.Nothing.value)(v.userBus)))(function () {
        return Rtsv2App_Capability_Navigate.navigate(navigateAppM)(Rtsv2App_Data_Route.Home.value);
      });
    });
  }), function () {
    var $17 = Effect_Class.liftEffect(monadEffectAppM);
    var $18 = Routing_Duplex.print(Rtsv2App_Data_Route.routeCodec);
    return function ($19) {
      return $17(Routing_Hash.setHash($18($19)));
    };
  }());
  exports["runAppM"] = runAppM;
  exports["monadAffAppM"] = monadAffAppM;
  exports["monadAskAppM"] = monadAskAppM;
  exports["nowAppM"] = nowAppM;
  exports["logMessagesAppM"] = logMessagesAppM;
  exports["navigateAppM"] = navigateAppM;
  exports["manageUserAppM"] = manageUserAppM;
})(PS);

(function ($PS) {
  "use strict";

  $PS["Rtsv2App.Component.HTML.Utils"] = $PS["Rtsv2App.Component.HTML.Utils"] || {};
  var exports = $PS["Rtsv2App.Component.HTML.Utils"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Unit = $PS["Data.Unit"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"]; // | PureScript is a strict language. If we want to conditionally display an element, then we
  // | should hide the evaluation behind a function, which won't be evaluated right away, in order
  // | to minimize the work performed each render.

  var whenElem = function whenElem(cond) {
    return function (f) {
      if (cond) {
        return f(Data_Unit.unit);
      }

      ;
      return Halogen_HTML_Core.text("");
    };
  }; // | We must provide a `String` to the "href" attribute, but we represent routes with the much
  // | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.


  var safeHref = function () {
    var $6 = Data_Semigroup.append(Data_Semigroup.semigroupString)("#");
    var $7 = Routing_Duplex.print(Rtsv2App_Data_Route.routeCodec);
    return function ($8) {
      return Halogen_HTML_Properties.href($6($7($8)));
    };
  }(); // | Sometimes we need to deal with elements which may or may not exist. This function lets us
  // | provide rendering for the element if it exists, and renders an empty node otherwise.


  var maybeElem = function maybeElem(v) {
    return function (v1) {
      if (v instanceof Data_Maybe.Just) {
        return v1(v.value0);
      }

      ;
      return Halogen_HTML_Core.text("");
    };
  }; // | custom data attribute pass name and it's value as `String`s


  var dataAttr = function dataAttr(atrName) {
    return function (atrVal) {
      return Halogen_HTML_Properties.attr(Halogen_HTML_Core.AttrName("data-" + atrName))(atrVal);
    };
  }; // | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
  // | a few characters all over our HTML.


  var css = function css($9) {
    return Halogen_HTML_Properties.class_(Halogen_HTML_Core.ClassName($9));
  };

  exports["css"] = css;
  exports["dataAttr"] = dataAttr;
  exports["safeHref"] = safeHref;
  exports["maybeElem"] = maybeElem;
  exports["whenElem"] = whenElem;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Component.HTML.Footer"] = $PS["Rtsv2App.Component.HTML.Footer"] || {};
  var exports = $PS["Rtsv2App.Component.HTML.Footer"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var footer = Halogen_HTML_Elements.footer([Rtsv2App_Component_HTML_Utils.css("footer footer-static footer-light navbar-border navbar-shadow")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("clearfix blue-grey lighten-2 text-sm-center mb-0 px-2")])([Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("float-md-left d-block d-md-inline-block")])([Halogen_HTML_Core.text("2020 \xa9 Copyright")]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("attribution")])([Halogen_HTML_Core.text("An interactive Admin area by "), Halogen_HTML_Elements.a([Halogen_HTML_Properties.href("https://www.id3as.com/")])([Halogen_HTML_Core.text("Id3as")]), Halogen_HTML_Core.text(".")])])]);
  exports["footer"] = footer;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Component.HTML.Header"] = $PS["Rtsv2App.Component.HTML.Header"] || {};
  var exports = $PS["Rtsv2App.Component.HTML.Header"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];

  var LogUserOut = function () {
    function LogUserOut() {}

    ;
    LogUserOut.value = new LogUserOut();
    return LogUserOut;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var component = function component(dictMonadAff) {
    return function (dictNavigate) {
      var render = function render(v) {
        return Halogen_HTML_Elements.nav([Rtsv2App_Component_HTML_Utils.css("header-navbar navbar-expand-md navbar navbar-with-menu navbar-without-dd-arrow fixed-top navbar-semi-light")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("navbar-wrapper")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("navbar-container content")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("collapse navbar-collapse show"), Halogen_HTML_Properties.id_("navbar-mobile")])([Halogen_HTML_Elements.ul([Rtsv2App_Component_HTML_Utils.css("nav navbar-nav mr-auto float-left")])([Halogen_HTML_Elements.li([Rtsv2App_Component_HTML_Utils.css("nav-item d-block d-md-none")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.css("nav-link nav-menu-main menu-toggle hidden-xs")])([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-menu")])([])])])]), Halogen_HTML_Elements.ul([Rtsv2App_Component_HTML_Utils.css("nav navbar-nav float-right")])([Halogen_HTML_Elements.li([Rtsv2App_Component_HTML_Utils.css("dropdown dropdown-user nav-item")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.css("dropdown-toggle nav-link dropdown-user-link"), Rtsv2App_Component_HTML_Utils.dataAttr("toggle")("dropdown")])([Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("avatar")])([Halogen_HTML_Elements.img([Halogen_HTML_Properties.src("assets/images/avatar-s.png")]), Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("")])([])])]), Rtsv2App_Component_HTML_Utils.maybeElem(v.currentUser)(function (cu) {
          return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("dropdown-menu dropdown-menu-right")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("arrow_box_right")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.css("dropdown-item"), Rtsv2App_Component_HTML_Utils.safeHref(new Rtsv2App_Data_Route.Profile(cu.username))])([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-user")])([]), Halogen_HTML_Core.text("Edit Profile")])])]);
        })])])])])])]);
      };

      var initialState = function initialState(v) {
        return {
          currentUser: v.currentUser,
          route: v.route
        };
      };

      var handleAction = function handleAction(v) {
        if (v instanceof Receive) {
          return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(v.value0);
        }

        ;

        if (v instanceof LogUserOut) {
          return Rtsv2App_Capability_Navigate.logout(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate));
        }

        ;
        throw new Error("Failed pattern match at Rtsv2App.Component.HTML.Header (line 54, column 18 - line 58, column 25): " + [v.constructor.name]);
      };

      return Halogen_Component.mkComponent({
        initialState: initialState,
        render: render,
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction,
          handleQuery: Halogen_Component.defaultEval.handleQuery,
          receive: function receive($14) {
            return Data_Maybe.Just.create(Receive.create($14));
          },
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Component.HTML.MainMenu"] = $PS["Rtsv2App.Component.HTML.MainMenu"] || {};
  var exports = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];

  var LogUserOut = function () {
    function LogUserOut() {}

    ;
    LogUserOut.value = new LogUserOut();
    return LogUserOut;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var component = function component(dictMonadAff) {
    return function (dictNavigate) {
      var render = function render(v) {
        var navItem = function navItem(r) {
          return function (html) {
            return Halogen_HTML_Elements.li([Rtsv2App_Component_HTML_Utils.css("" + Data_Monoid.guard(Data_Monoid.monoidString)(Data_Eq.eq(Rtsv2App_Data_Route.eqRoute)(v.route)(r))("active"))])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.safeHref(r)])(html)]);
          };
        };

        return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main-menu menu-fixed menu-light menu-accordion menu-shadow "), Rtsv2App_Component_HTML_Utils.dataAttr("scroll-to-active")("true"), Rtsv2App_Component_HTML_Utils.dataAttr("img")("assets/images/backgrounds/02.jpg")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("navbar-header")])([Halogen_HTML_Elements.ul([Rtsv2App_Component_HTML_Utils.css("nav navbar-nav flex-row")])([Halogen_HTML_Elements.li([Rtsv2App_Component_HTML_Utils.css("nav-item mr-auto")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.css("navbar-brand"), Rtsv2App_Component_HTML_Utils.safeHref(Rtsv2App_Data_Route.Home.value)])([Halogen_HTML_Elements.img([Rtsv2App_Component_HTML_Utils.css("brand-logo"), Halogen_HTML_Properties.src("assets/images/logo/logo.png")]), Halogen_HTML_Elements.h3([Rtsv2App_Component_HTML_Utils.css("brand-text")])([Halogen_HTML_Core.text("Limelight Admin")])])]), Halogen_HTML_Elements.li([Rtsv2App_Component_HTML_Utils.css("nav-item d-md-none")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.css("nav-link close-navbar")])([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-x")])([])])])])]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main-menu-content")])([Halogen_HTML_Elements.ul([Rtsv2App_Component_HTML_Utils.css("navigation navigation-main"), Halogen_HTML_Properties.id_("main-menu-navigation")])([navItem(Rtsv2App_Data_Route.Home.value)([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-home")])([]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("menu-title")])([Halogen_HTML_Core.text("Dashboard")])]), Rtsv2App_Component_HTML_Utils.whenElem(!Data_Maybe.isJust(v.currentUser))(function (v1) {
          return navItem(Rtsv2App_Data_Route.Login.value)([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-lock")])([]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("menu-title")])([Halogen_HTML_Core.text("Login")])]);
        }), Rtsv2App_Component_HTML_Utils.whenElem(Data_Maybe.isJust(v.currentUser))(function (v1) {
          return navItem(Rtsv2App_Data_Route.Settings.value)([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-settings")])([]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("menu-title")])([Halogen_HTML_Core.text("Settings")])]);
        }), Rtsv2App_Component_HTML_Utils.maybeElem(v.currentUser)(function (c) {
          return navItem(new Rtsv2App_Data_Route.Profile(c.username))([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-user")])([]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("menu-title")])([Halogen_HTML_Core.text("Profile")])]);
        }), Rtsv2App_Component_HTML_Utils.whenElem(Data_Maybe.isJust(v.currentUser))(function (v1) {
          return Halogen_HTML_Elements.li([])([Halogen_HTML_Elements.a([Halogen_HTML_Events.onClick(function (v2) {
            return new Data_Maybe.Just(LogUserOut.value);
          })])([Halogen_HTML_Elements.i([Rtsv2App_Component_HTML_Utils.css("ft-lock")])([]), Halogen_HTML_Elements.span([Rtsv2App_Component_HTML_Utils.css("menu-title")])([Halogen_HTML_Core.text("Logout")])])]);
        })])])]);
      };

      var initialState = function initialState(v) {
        return {
          currentUser: v.currentUser,
          route: v.route
        };
      };

      var handleAction = function handleAction(v) {
        if (v instanceof Receive) {
          return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(v.value0);
        }

        ;

        if (v instanceof LogUserOut) {
          return Rtsv2App_Capability_Navigate.logout(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate));
        }

        ;
        throw new Error("Failed pattern match at Rtsv2App.Component.HTML.MainMenu (line 55, column 18 - line 59, column 25): " + [v.constructor.name]);
      };

      return Halogen_Component.mkComponent({
        initialState: initialState,
        render: render,
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction,
          handleQuery: Halogen_Component.defaultEval.handleQuery,
          receive: function receive($18) {
            return Data_Maybe.Just.create(Receive.create($18));
          },
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Page.Home"] = $PS["Rtsv2App.Page.Home"] || {};
  var exports = $PS["Rtsv2App.Page.Home"];
  var Component_HOC_Connect = $PS["Component.HOC.Connect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Rtsv2App_Component_HTML_Footer = $PS["Rtsv2App.Component.HTML.Footer"];
  var Rtsv2App_Component_HTML_Header = $PS["Rtsv2App.Component.HTML.Header"];
  var Rtsv2App_Component_HTML_MainMenu = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];

  var Initialize = function () {
    function Initialize() {}

    ;
    Initialize.value = new Initialize();
    return Initialize;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var component = function component(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (dictNavigate) {
        var render = function render(v) {
          var html = [Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-body")])([Halogen_HTML_Elements.img([Rtsv2App_Component_HTML_Utils.css("col-12"), Halogen_HTML_Properties.src("assets/images/backgrounds/world-map.png")])])];
          return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
            return "header";
          }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_Header.component(dictMonadAff)(dictNavigate))({
            currentUser: v.currentUser,
            route: Rtsv2App_Data_Route.Login.value
          })(Data_Void.absurd), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
            return "mainMenu";
          }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_MainMenu.component(dictMonadAff)(dictNavigate))({
            currentUser: v.currentUser,
            route: Rtsv2App_Data_Route.Home.value
          })(Data_Void.absurd), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("app-content content")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper-before")])([]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header-left col-md-4 col-12 mb-2")])([Halogen_HTML_Elements.h3([Rtsv2App_Component_HTML_Utils.css("content-header-h3")])([Halogen_HTML_Core.text("Home")])])]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-body")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-12")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card")])(html)])])])])]), Rtsv2App_Component_HTML_Footer.footer]);
        };

        var initialState = function initialState(v) {
          return {
            currentUser: v.currentUser,
            page: 1
          };
        };

        var handleAction = function handleAction(v) {
          if (v instanceof Initialize) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
              if (v1.currentUser instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }

              ;
              return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
            });
          }

          ;

          if (v instanceof Receive) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
              var $16 = {};

              for (var $17 in v1) {
                if ({}.hasOwnProperty.call(v1, $17)) {
                  $16[$17] = v1[$17];
                }

                ;
              }

              ;
              $16.currentUser = v.value0.currentUser;
              return $16;
            });
          }

          ;
          throw new Error("Failed pattern match at Rtsv2App.Page.Home (line 59, column 18 - line 67, column 48): " + [v.constructor.name]);
        };

        return Component_HOC_Connect.component(dictMonadAff)(dictMonadAsk)()(Halogen_Component.mkComponent({
          initialState: initialState,
          render: render,
          "eval": Halogen_Component.mkEval({
            handleAction: handleAction,
            handleQuery: Halogen_Component.defaultEval.handleQuery,
            receive: function receive($21) {
              return Data_Maybe.Just.create(Receive.create($21));
            },
            initialize: new Data_Maybe.Just(Initialize.value),
            finalize: Halogen_Component.defaultEval.finalize
          })
        }));
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Form.Validation"] = $PS["Rtsv2App.Form.Validation"] || {};
  var exports = $PS["Rtsv2App.Form.Validation"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_String_CodePoints = $PS["Data.String.CodePoints"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Formless_Validation = $PS["Formless.Validation"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Email = $PS["Rtsv2App.Data.Email"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];

  var Required = function () {
    function Required() {}

    ;
    Required.value = new Required();
    return Required;
  }();

  var TooShort = function () {
    function TooShort() {}

    ;
    TooShort.value = new TooShort();
    return TooShort;
  }();

  var TooLong = function () {
    function TooLong() {}

    ;
    TooLong.value = new TooLong();
    return TooLong;
  }();

  var InvalidEmail = function () {
    function InvalidEmail() {}

    ;
    InvalidEmail.value = new InvalidEmail();
    return InvalidEmail;
  }();

  var InvalidUsername = function () {
    function InvalidUsername() {}

    ;
    InvalidUsername.value = new InvalidUsername();
    return InvalidUsername;
  }();

  var InvalidAvatar = function () {
    function InvalidAvatar() {}

    ;
    InvalidAvatar.value = new InvalidAvatar();
    return InvalidAvatar;
  }();

  var usernameFormat = function usernameFormat(dictMonad) {
    return Formless_Validation.hoistFnE_(dictMonad)(function () {
      var $17 = Data_Either.note(InvalidUsername.value);
      return function ($18) {
        return $17(Rtsv2App_Data_Username.parse($18));
      };
    }());
  };

  var toOptional = function toOptional(dictMonoid) {
    return function (dictEq) {
      return function (dictMonad) {
        return function (v) {
          return function (form) {
            return function (val) {
              var v1 = Data_Eq.eq(dictEq)(val)(Data_Monoid.mempty(dictMonoid));

              if (v1) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Control_Applicative.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value));
              }

              ;
              return Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(Data_Functor.map(Data_Either.functorEither)(Data_Maybe.Just.create))(Formless_Validation.runValidation(dictMonad)(v)(form)(val));
            };
          };
        };
      };
    };
  };

  var errorToString = function errorToString(v) {
    if (v instanceof Required) {
      return "This field is required.";
    }

    ;

    if (v instanceof TooShort) {
      return "Not enough characters entered";
    }

    ;

    if (v instanceof TooLong) {
      return "Too many characters entered";
    }

    ;

    if (v instanceof InvalidEmail) {
      return "Invalid email address";
    }

    ;

    if (v instanceof InvalidUsername) {
      return "Invalid username";
    }

    ;

    if (v instanceof InvalidAvatar) {
      return "Invalid image URL";
    }

    ;
    throw new Error("Failed pattern match at Rtsv2App.Form.Validation (line 39, column 17 - line 45, column 39): " + [v.constructor.name]);
  };

  var cond = function cond(f) {
    return function (err) {
      return function (a) {
        var $16 = f(a);

        if ($16) {
          return Control_Applicative.pure(Data_Either.applicativeEither)(a);
        }

        ;
        return new Data_Either.Left(err);
      };
    };
  };

  var emailFormat = function emailFormat(dictMonad) {
    return Formless_Validation.hoistFnE_(dictMonad)(function () {
      var $19 = Data_Functor.map(Data_Either.functorEither)(Rtsv2App_Data_Email.Email);
      var $20 = cond(Data_String_CodeUnits.contains("@"))(InvalidEmail.value);
      return function ($21) {
        return $19($20($21));
      };
    }());
  };

  var maxLength = function maxLength(dictMonad) {
    return function (n) {
      return Formless_Validation.hoistFnE_(dictMonad)(cond(function (str) {
        return Data_String_CodePoints.length(str) <= n;
      })(TooLong.value));
    };
  };

  var minLength = function minLength(dictMonad) {
    return function (n) {
      return Formless_Validation.hoistFnE_(dictMonad)(cond(function (str) {
        return Data_String_CodePoints.length(str) > n;
      })(TooShort.value));
    };
  };

  var required = function required(dictEq) {
    return function (dictMonoid) {
      return function (dictMonad) {
        return Formless_Validation.hoistFnE_(dictMonad)(cond(function (v) {
          return Data_Eq.notEq(dictEq)(v)(Data_Monoid.mempty(dictMonoid));
        })(Required.value));
      };
    };
  };

  var avatarFormat = function avatarFormat(dictMonad) {
    return Formless_Validation.hoistFnE_(dictMonad)(function () {
      var $22 = Data_Either.note(InvalidAvatar.value);
      return function ($23) {
        return $22(Rtsv2App_Data_Avatar.parse($23));
      };
    }());
  };

  exports["errorToString"] = errorToString;
  exports["required"] = required;
  exports["minLength"] = minLength;
  exports["maxLength"] = maxLength;
  exports["emailFormat"] = emailFormat;
  exports["usernameFormat"] = usernameFormat;
  exports["avatarFormat"] = avatarFormat;
  exports["toOptional"] = toOptional;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Form.Field"] = $PS["Rtsv2App.Form.Field"] || {};
  var exports = $PS["Rtsv2App.Form.Field"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Formless_Action = $PS["Formless.Action"];
  var Formless_Retrieve = $PS["Formless.Retrieve"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Form_Validation = $PS["Rtsv2App.Form.Validation"];

  var submit = function submit(buttonText) {
    return Halogen_HTML_Elements.button([Rtsv2App_Component_HTML_Utils.css("btn btn-lg btn-primary pull-xs-right"), Halogen_HTML_Events.onClick(function (v) {
      return new Data_Maybe.Just(Formless_Action.submit);
    })])([Halogen_HTML_Core.text(buttonText)]);
  };

  var input = function input(dictIsSymbol) {
    return function (dictNewtype) {
      return function (dictNewtype1) {
        return function (dictCons) {
          return function (dictCons1) {
            return function (sym) {
              return function (form) {
                return function (props) {
                  return Halogen_HTML_Elements.fieldset([Rtsv2App_Component_HTML_Utils.css("form-group")])([Halogen_HTML_Elements.input(Data_Semigroup.append(Data_Semigroup.semigroupArray)([Rtsv2App_Component_HTML_Utils.css("form-control form-control-lg"), Halogen_HTML_Properties.value(Formless_Retrieve.getInput(dictIsSymbol)(dictNewtype)()(sym)(form)), Halogen_HTML_Events.onValueInput(function () {
                    var $6 = Formless_Action.setValidate(dictIsSymbol)(dictNewtype1)()(sym);
                    return function ($7) {
                      return Data_Maybe.Just.create($6($7));
                    };
                  }())])(props)), Rtsv2App_Component_HTML_Utils.maybeElem(Formless_Retrieve.getError(dictIsSymbol)(dictNewtype)()(sym)(form))(function (err) {
                    return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("error-messages")])([Halogen_HTML_Core.text(Rtsv2App_Form_Validation.errorToString(err))]);
                  })]);
                };
              };
            };
          };
        };
      };
    };
  };

  exports["submit"] = submit;
  exports["input"] = input;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Page.Login"] = $PS["Rtsv2App.Page.Login"] || {};
  var exports = $PS["Rtsv2App.Page.Login"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Formless_Class_Initial = $PS["Formless.Class.Initial"];
  var Formless_Component = $PS["Formless.Component"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Formless_Query = $PS["Formless.Query"];
  var Formless_Transform_Record = $PS["Formless.Transform.Record"];
  var Formless_Transform_Row = $PS["Formless.Transform.Row"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var Formless_Validation = $PS["Formless.Validation"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Heterogeneous_Mapping = $PS["Heterogeneous.Mapping"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Capability_Resource_User = $PS["Rtsv2App.Capability.Resource.User"];
  var Rtsv2App_Component_HTML_Footer = $PS["Rtsv2App.Component.HTML.Footer"];
  var Rtsv2App_Component_HTML_Header = $PS["Rtsv2App.Component.HTML.Header"];
  var Rtsv2App_Component_HTML_MainMenu = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Form_Field = $PS["Rtsv2App.Form.Field"];
  var Rtsv2App_Form_Validation = $PS["Rtsv2App.Form.Validation"];

  var LoginForm = function LoginForm(x) {
    return x;
  };

  var SetLoginError = function () {
    function SetLoginError(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    SetLoginError.create = function (value0) {
      return function (value1) {
        return new SetLoginError(value0, value1);
      };
    };

    return SetLoginError;
  }();

  var HandleLoginForm = function () {
    function HandleLoginForm(value0) {
      this.value0 = value0;
    }

    ;

    HandleLoginForm.create = function (value0) {
      return new HandleLoginForm(value0);
    };

    return HandleLoginForm;
  }();

  var newtypeLoginForm = new Data_Newtype.Newtype(function (n) {
    return n;
  }, LoginForm);
  var functorFormQuery = new Data_Functor.Functor(function (f) {
    return function (m) {
      return new SetLoginError(m.value0, f(m.value1));
    };
  });

  var formComponent = function formComponent(dictMonadAff) {
    var proxies = Formless_Transform_Row.mkSProxies()(newtypeLoginForm)(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Transform_Row.makeSProxiesNil)))(Formless_Types_Form.FormProxy.value);

    var renderLogin = function renderLogin(v) {
      return Halogen_HTML_Elements.form_([Rtsv2App_Component_HTML_Utils.whenElem(v.loginError)(function (v1) {
        return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("error-messages")])([Halogen_HTML_Core.text("Email or password is invalid")]);
      }), Halogen_HTML_Elements.fieldset_([Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
        return "email";
      }))(newtypeLoginForm)(newtypeLoginForm)()()(proxies.email)(v.form)([Halogen_HTML_Properties.placeholder("Email"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputEmail.value)]), Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
        return "password";
      }))(newtypeLoginForm)(newtypeLoginForm)()()(proxies.password)(v.form)([Halogen_HTML_Properties.placeholder("Password"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputPassword.value)]), Rtsv2App_Form_Field.submit("Log in")])]);
    };

    var handleQuery = function handleQuery(v) {
      return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
        var $29 = {};

        for (var $30 in v1) {
          if ({}.hasOwnProperty.call(v1, $30)) {
            $29[$30] = v1[$30];
          }

          ;
        }

        ;
        $29.loginError = v.value0;
        return $29;
      }))(function () {
        return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value1));
      });
    };

    var formInput = function formInput(v) {
      return {
        validators: {
          email: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictMonadAff.MonadEffect0().Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictMonadAff.MonadEffect0().Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictMonadAff.MonadEffect0().Monad0()))(Rtsv2App_Form_Validation.minLength(dictMonadAff.MonadEffect0().Monad0())(3))(Rtsv2App_Form_Validation.emailFormat(dictMonadAff.MonadEffect0().Monad0()))),
          password: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictMonadAff.MonadEffect0().Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictMonadAff.MonadEffect0().Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictMonadAff.MonadEffect0().Monad0()))(Rtsv2App_Form_Validation.minLength(dictMonadAff.MonadEffect0().Monad0())(2))(Rtsv2App_Form_Validation.maxLength(dictMonadAff.MonadEffect0().Monad0())(20)))
        },
        initialInputs: Data_Maybe.Nothing.value,
        loginError: false
      };
    };

    return Formless_Component.component(dictMonadAff)()()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.inputFieldsToFormFieldsNil)())())(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.inputFieldsToInputNil)())())(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.nilCountErrors)))(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.nilAllTouched)))(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.setFormFieldsTouchedNil)())())(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedNil)))(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllNil)))(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))(dictMonadAff.MonadEffect0().Monad0())()(newtypeLoginForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))(dictMonadAff.MonadEffect0().Monad0())()(newtypeLoginForm)()()(Formless_Internal_Transform.applyToValidationNil(dictMonadAff.MonadEffect0().Monad0()))))(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))()(Formless_Internal_Transform.formFieldsToMaybeOutputNil)())())(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
      return "email";
    }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
      return "password";
    }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowNil)())())(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)(newtypeLoginForm)()()()()()()()()()(formInput)({
      render: renderLogin,
      handleAction: Formless_Component.defaultSpec.handleAction,
      handleQuery: handleQuery,
      handleEvent: Formless_Component.raiseResult(newtypeLoginForm)(Heterogeneous_Mapping.hmapRecord()(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
        return "email";
      }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
        return "password";
      }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexNil)()())()())),
      receive: Formless_Component.defaultSpec.receive,
      initialize: Formless_Component.defaultSpec.initialize,
      finalize: Formless_Component.defaultSpec.finalize
    });
  };

  var component = function component(dictMonadAff) {
    return function (dictNavigate) {
      return function (dictManageUser) {
        var render = function render(v) {
          var html = [Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-header")])([Halogen_HTML_Core.text("Sign In")]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-content collapse show")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-body")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.safeHref(Rtsv2App_Data_Route.Register.value)])([Halogen_HTML_Core.text("Need an account?")]), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
            return "formless";
          }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(formComponent(dictMonadAff))(Data_Unit.unit)(function ($39) {
            return Data_Maybe.Just.create(HandleLoginForm.create($39));
          })])])];
          return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
            return "header";
          }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_Header.component(dictMonadAff)(dictNavigate))({
            currentUser: Data_Maybe.Nothing.value,
            route: Rtsv2App_Data_Route.Login.value
          })(Data_Void.absurd), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
            return "mainMenu";
          }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_MainMenu.component(dictMonadAff)(dictNavigate))({
            currentUser: Data_Maybe.Nothing.value,
            route: Rtsv2App_Data_Route.Login.value
          })(Data_Void.absurd), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("app-content content")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper-before")])([]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header-left col-md-4 col-12 mb-2")])([Halogen_HTML_Elements.h3([Rtsv2App_Component_HTML_Utils.css("content-header-h3")])([Halogen_HTML_Core.text("Login")])])]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-body")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-12")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card")])(html)])])])])]), Rtsv2App_Component_HTML_Footer.footer]);
        };

        var handleAction = function handleAction(v) {
          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.loginUser(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser))(v.value0))(function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
              return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
                return "formless";
              }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(Formless_Query.injQuery(functorFormQuery)(new SetLoginError(true, Data_Unit.unit))));
            }

            ;

            if (v1 instanceof Data_Maybe.Just) {
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
                return "formless";
              }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(Formless_Query.injQuery(functorFormQuery)(new SetLoginError(false, Data_Unit.unit)))))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v2) {
                  return Control_Applicative.when(Halogen_Query_HalogenM.applicativeHalogenM)(v2.redirect)(Rtsv2App_Capability_Navigate.navigate(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate))(Rtsv2App_Data_Route.Home.value));
                });
              });
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Page.Login (line 61, column 28 - line 67, column 43): " + [v1.constructor.name]);
          });
        };

        return Halogen_Component.mkComponent({
          initialState: Control_Category.identity(Control_Category.categoryFn),
          render: render,
          "eval": Halogen_Component.mkEval({
            handleAction: handleAction,
            handleQuery: Halogen_Component.defaultEval.handleQuery,
            receive: Halogen_Component.defaultEval.receive,
            initialize: Halogen_Component.defaultEval.initialize,
            finalize: Halogen_Component.defaultEval.finalize
          })
        });
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Network.RemoteData"] = $PS["Network.RemoteData"] || {};
  var exports = $PS["Network.RemoteData"];
  var Data_Either = $PS["Data.Either"];
  var Data_Lens_Prism = $PS["Data.Lens.Prism"];
  var Data_Maybe = $PS["Data.Maybe"];

  var NotAsked = function () {
    function NotAsked() {}

    ;
    NotAsked.value = new NotAsked();
    return NotAsked;
  }();

  var Loading = function () {
    function Loading() {}

    ;
    Loading.value = new Loading();
    return Loading;
  }();

  var Success = function () {
    function Success(value0) {
      this.value0 = value0;
    }

    ;

    Success.create = function (value0) {
      return new Success(value0);
    };

    return Success;
  }();

  var toMaybe = function toMaybe(v) {
    if (v instanceof Success) {
      return new Data_Maybe.Just(v.value0);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var fromMaybe = function fromMaybe(v) {
    if (v instanceof Data_Maybe.Nothing) {
      return NotAsked.value;
    }

    ;

    if (v instanceof Data_Maybe.Just) {
      return new Success(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Network.RemoteData (line 129, column 1 - line 129, column 51): " + [v.constructor.name]);
  };

  var _Success = function _Success(dictChoice) {
    var unwrap = function unwrap(v) {
      if (v instanceof Success) {
        return new Data_Either.Right(v.value0);
      }

      ;
      return new Data_Either.Left(v);
    };

    return Data_Lens_Prism.prism(Success.create)(unwrap)(dictChoice);
  };

  exports["NotAsked"] = NotAsked;
  exports["Loading"] = Loading;
  exports["toMaybe"] = toMaybe;
  exports["fromMaybe"] = fromMaybe;
  exports["_Success"] = _Success;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Page.Profile"] = $PS["Rtsv2App.Page.Profile"] || {};
  var exports = $PS["Rtsv2App.Page.Profile"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Lens_Record = $PS["Data.Lens.Record"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Network_RemoteData = $PS["Network.RemoteData"];
  var Rtsv2App_Capability_Resource_User = $PS["Rtsv2App.Capability.Resource.User"];
  var Rtsv2App_Component_HTML_Footer = $PS["Rtsv2App.Component.HTML.Footer"];
  var Rtsv2App_Component_HTML_Header = $PS["Rtsv2App.Component.HTML.Header"];
  var Rtsv2App_Component_HTML_MainMenu = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];

  var Initialize = function () {
    function Initialize() {}

    ;
    Initialize.value = new Initialize();
    return Initialize;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var LoadAuthor = function () {
    function LoadAuthor() {}

    ;
    LoadAuthor.value = new LoadAuthor();
    return LoadAuthor;
  }();

  var component = function component(dictMonadAff) {
    return function (dictNavigate) {
      return function (dictMonadAsk) {
        return function (dictManageUser) {
          var userInfo = function userInfo(state) {
            return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("user-info")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("container")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-xs-12 col-md-10 offset-md-1")])([Halogen_HTML_Elements.img([Rtsv2App_Component_HTML_Utils.css("user-img")]), Halogen_HTML_Elements.h4_([Halogen_HTML_Core.text(Rtsv2App_Data_Username.toString(state.username))]), Rtsv2App_Component_HTML_Utils.maybeElem(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(function (v) {
              return v.bio;
            })(Network_RemoteData.toMaybe(state.author)))(function (str) {
              return Halogen_HTML_Elements.p_([Halogen_HTML_Core.text(str)]);
            })])])])]);
          };

          var mainView = function mainView(state) {
            return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-xs-12 col-md-10 offset-md-1")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("articles-toggle")])([Halogen_HTML_Elements.ul([Rtsv2App_Component_HTML_Utils.css("nav nav-pills outline-active")])([])])]);
          };

          var render = function render(v) {
            return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "header";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_Header.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Register.value
            })(Data_Void.absurd), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "mainMenu";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_MainMenu.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Register.value
            })(Data_Void.absurd), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("profile-page")])([userInfo(v), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("container")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([mainView(v)])])]), Rtsv2App_Component_HTML_Footer.footer]);
          };

          var initialState = function initialState(v) {
            return {
              author: Network_RemoteData.NotAsked.value,
              currentUser: Data_Maybe.Nothing.value,
              username: v.username
            };
          };

          var handleAction = function handleAction(v) {
            if (v instanceof Initialize) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Bind.bindFlipped(Halogen_Query_HalogenM.bindHalogenM)(function () {
                var $44 = Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()));
                return function ($45) {
                  return $44(Effect_Ref.read($45));
                };
              }())(Control_Monad_Reader_Class.asks(Halogen_Query_HalogenM.monadAskHalogenM(dictMonadAsk))(function (v1) {
                return v1.userEnv.currentUser;
              })))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v2) {
                  var $26 = {};

                  for (var $27 in v2) {
                    if ({}.hasOwnProperty.call(v2, $27)) {
                      $26[$27] = v2[$27];
                    }

                    ;
                  }

                  ;
                  $26.currentUser = v1;
                  return $26;
                }))(function (v2) {
                  return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.fork(handleAction(LoadAuthor.value)));
                });
              });
            }

            ;

            if (v instanceof Receive) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                return Control_Applicative.when(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Eq.notEq(Rtsv2App_Data_Username.eqUsername)(v1.username)(v.value0.username))(Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v2) {
                  var $31 = {};

                  for (var $32 in v2) {
                    if ({}.hasOwnProperty.call(v2, $32)) {
                      $31[$32] = v2[$32];
                    }

                    ;
                  }

                  ;
                  $31.username = v.value0.username;
                  return $31;
                }))(function () {
                  return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.fork(handleAction(Initialize.value)));
                }));
              });
            }

            ;

            if (v instanceof LoadAuthor) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                var $36 = {};

                for (var $37 in v1) {
                  if ({}.hasOwnProperty.call(v1, $37)) {
                    $36[$37] = v1[$37];
                  }

                  ;
                }

                ;
                $36.author = Network_RemoteData.Loading.value;
                return $36;
              }))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.getAuthor(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser))(v1.username))(function (v2) {
                  return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                    var $41 = {};

                    for (var $42 in v3) {
                      if ({}.hasOwnProperty.call(v3, $42)) {
                        $41[$42] = v3[$42];
                      }

                      ;
                    }

                    ;
                    $41.author = Network_RemoteData.fromMaybe(v2);
                    return $41;
                  });
                });
              });
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Page.Profile (line 73, column 18 - line 88, column 48): " + [v.constructor.name]);
          };

          var _author = function _author(dictWander) {
            var $46 = Data_Lens_Record.prop(new Data_Symbol.IsSymbol(function () {
              return "author";
            }))()()(Data_Symbol.SProxy.value)(dictWander.Strong0());
            var $47 = Network_RemoteData["_Success"](dictWander.Choice1());
            return function ($48) {
              return $46($47($48));
            };
          };

          return Halogen_Component.mkComponent({
            initialState: initialState,
            render: render,
            "eval": Halogen_Component.mkEval({
              handleAction: handleAction,
              handleQuery: Halogen_Component.defaultEval.handleQuery,
              receive: function receive($49) {
                return Data_Maybe.Just.create(Receive.create($49));
              },
              initialize: new Data_Maybe.Just(Initialize.value),
              finalize: Halogen_Component.defaultEval.finalize
            })
          });
        };
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Page.Register"] = $PS["Rtsv2App.Page.Register"] || {};
  var exports = $PS["Rtsv2App.Page.Register"];
  var Component_HOC_Connect = $PS["Component.HOC.Connect"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Formless_Class_Initial = $PS["Formless.Class.Initial"];
  var Formless_Component = $PS["Formless.Component"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Formless_Transform_Record = $PS["Formless.Transform.Record"];
  var Formless_Transform_Row = $PS["Formless.Transform.Row"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var Formless_Validation = $PS["Formless.Validation"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Heterogeneous_Mapping = $PS["Heterogeneous.Mapping"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Capability_Resource_User = $PS["Rtsv2App.Capability.Resource.User"];
  var Rtsv2App_Component_HTML_Footer = $PS["Rtsv2App.Component.HTML.Footer"];
  var Rtsv2App_Component_HTML_Header = $PS["Rtsv2App.Component.HTML.Header"];
  var Rtsv2App_Component_HTML_MainMenu = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Form_Field = $PS["Rtsv2App.Form.Field"];
  var Rtsv2App_Form_Validation = $PS["Rtsv2App.Form.Validation"];

  var RegisterForm = function RegisterForm(x) {
    return x;
  };

  var HandleRegisterForm = function () {
    function HandleRegisterForm(value0) {
      this.value0 = value0;
    }

    ;

    HandleRegisterForm.create = function (value0) {
      return new HandleRegisterForm(value0);
    };

    return HandleRegisterForm;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var newtypeRegisterForm = new Data_Newtype.Newtype(function (n) {
    return n;
  }, RegisterForm);

  var component = function component(dictMonadAff) {
    return function (dictManageUser) {
      return function (dictMonadAsk) {
        return function (dictNavigate) {
          var render = function render(v) {
            var formComponent = function () {
              var renderForm = function renderForm(v1) {
                var proxies = Formless_Transform_Row.mkSProxies()(newtypeRegisterForm)(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))()(Formless_Transform_Row.makeSProxiesNil))))(Formless_Types_Form.FormProxy.value);
                var username = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))(newtypeRegisterForm)(newtypeRegisterForm)()()(proxies.username)(v1.form)([Halogen_HTML_Properties.placeholder("Username"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputText.value)]);
                var password = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))(newtypeRegisterForm)(newtypeRegisterForm)()()(proxies.password)(v1.form)([Halogen_HTML_Properties.placeholder("Password"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputPassword.value)]);
                var email = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))(newtypeRegisterForm)(newtypeRegisterForm)()()(proxies.email)(v1.form)([Halogen_HTML_Properties.placeholder("Email"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputEmail.value)]);
                return Halogen_HTML_Elements.form_([Halogen_HTML_Elements.fieldset_([username, email, password]), Rtsv2App_Form_Field.submit("Sign up")]);
              };

              var formInput = function formInput(v1) {
                return {
                  validators: {
                    username: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.usernameFormat(dictManageUser.Monad0())),
                    email: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictManageUser.Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.minLength(dictManageUser.Monad0())(3))(Rtsv2App_Form_Validation.emailFormat(dictManageUser.Monad0()))),
                    password: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictManageUser.Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.minLength(dictManageUser.Monad0())(8))(Rtsv2App_Form_Validation.maxLength(dictManageUser.Monad0())(20)))
                  },
                  initialInputs: Data_Maybe.Nothing.value
                };
              };

              return Formless_Component.component(dictMonadAff)()()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsNil)())())())(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.inputFieldsToInputNil)())())())(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.nilCountErrors))))(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.nilAllTouched))))(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedNil)())())())(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedNil))))(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllNil))))(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(dictManageUser.Monad0())()(newtypeRegisterForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(dictManageUser.Monad0())()(newtypeRegisterForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(dictManageUser.Monad0())()(newtypeRegisterForm)()()(Formless_Internal_Transform.applyToValidationNil(dictManageUser.Monad0())))))(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputNil)())())())(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowNil)())())())(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)(newtypeRegisterForm)()()()()()()()()()(formInput)({
                render: renderForm,
                handleAction: Formless_Component.defaultSpec.handleAction,
                handleQuery: Formless_Component.defaultSpec.handleQuery,
                handleEvent: Formless_Component.raiseResult(newtypeRegisterForm)(Heterogeneous_Mapping.hmapRecord()(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexNil)()())()())()())),
                receive: Formless_Component.defaultSpec.receive,
                initialize: Formless_Component.defaultSpec.initialize,
                finalize: Formless_Component.defaultSpec.finalize
              });
            }();

            var html = [Halogen_HTML_Elements.h1([Rtsv2App_Component_HTML_Utils.css("text-xs-center")])([Halogen_HTML_Core.text("Sign Up")]), Halogen_HTML_Elements.p([Rtsv2App_Component_HTML_Utils.css("text-xs-center")])([Halogen_HTML_Elements.a([Rtsv2App_Component_HTML_Utils.safeHref(Rtsv2App_Data_Route.Login.value)])([Halogen_HTML_Core.text("Already have an account?")])]), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "formless";
            }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(formComponent)(Data_Unit.unit)(function ($29) {
              return Data_Maybe.Just.create(HandleRegisterForm.create($29));
            })];
            return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "header";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_Header.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Register.value
            })(Data_Void.absurd), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "mainMenu";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_MainMenu.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Register.value
            })(Data_Void.absurd), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("app-content content")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper-before")])([]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header-left col-md-4 col-12 mb-2")])([Halogen_HTML_Elements.h3([Rtsv2App_Component_HTML_Utils.css("content-header-h3")])([Halogen_HTML_Core.text("Register")])])]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-body")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-12")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card")])(html)])])])])]), Rtsv2App_Component_HTML_Footer.footer]);
          };

          var initialState = function initialState(v) {
            return {
              currentUser: v.currentUser
            };
          };

          var handleAction = function handleAction(v) {
            if (v instanceof HandleRegisterForm) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.registerUser(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser))(v.value0))(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(function (v1) {
                return Rtsv2App_Capability_Navigate.navigate(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate))(Rtsv2App_Data_Route.Home.value);
              }));
            }

            ;

            if (v instanceof Receive) {
              return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                var $24 = {};

                for (var $25 in v1) {
                  if ({}.hasOwnProperty.call(v1, $25)) {
                    $24[$25] = v1[$25];
                  }

                  ;
                }

                ;
                $24.currentUser = v.value0.currentUser;
                return $24;
              });
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Page.Register (line 75, column 18 - line 79, column 48): " + [v.constructor.name]);
          };

          return Component_HOC_Connect.component(dictMonadAff)(dictMonadAsk)()(Halogen_Component.mkComponent({
            initialState: initialState,
            render: render,
            "eval": Halogen_Component.mkEval({
              handleAction: handleAction,
              handleQuery: Halogen_Component.defaultEval.handleQuery,
              receive: function receive($30) {
                return Data_Maybe.Just.create(Receive.create($30));
              },
              initialize: Halogen_Component.defaultEval.initialize,
              finalize: Halogen_Component.defaultEval.finalize
            })
          }));
        };
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Page.Settings"] = $PS["Rtsv2App.Page.Settings"] || {};
  var exports = $PS["Rtsv2App.Page.Settings"];
  var Component_HOC_Connect = $PS["Component.HOC.Connect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Reader_Class = $PS["Control.Monad.Reader.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Formless_Action = $PS["Formless.Action"];
  var Formless_Class_Initial = $PS["Formless.Class.Initial"];
  var Formless_Component = $PS["Formless.Component"];
  var Formless_Internal_Transform = $PS["Formless.Internal.Transform"];
  var Formless_Query = $PS["Formless.Query"];
  var Formless_Retrieve = $PS["Formless.Retrieve"];
  var Formless_Transform_Record = $PS["Formless.Transform.Record"];
  var Formless_Transform_Row = $PS["Formless.Transform.Row"];
  var Formless_Types_Component = $PS["Formless.Types.Component"];
  var Formless_Types_Form = $PS["Formless.Types.Form"];
  var Formless_Validation = $PS["Formless.Validation"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Heterogeneous_Mapping = $PS["Heterogeneous.Mapping"];
  var Network_RemoteData = $PS["Network.RemoteData"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Capability_Resource_User = $PS["Rtsv2App.Capability.Resource.User"];
  var Rtsv2App_Component_HTML_Footer = $PS["Rtsv2App.Component.HTML.Footer"];
  var Rtsv2App_Component_HTML_Header = $PS["Rtsv2App.Component.HTML.Header"];
  var Rtsv2App_Component_HTML_MainMenu = $PS["Rtsv2App.Component.HTML.MainMenu"];
  var Rtsv2App_Component_HTML_Utils = $PS["Rtsv2App.Component.HTML.Utils"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Email = $PS["Rtsv2App.Data.Email"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];
  var Rtsv2App_Form_Field = $PS["Rtsv2App.Form.Field"];
  var Rtsv2App_Form_Validation = $PS["Rtsv2App.Form.Validation"];

  var SettingsForm = function SettingsForm(x) {
    return x;
  };

  var Initialize = function () {
    function Initialize() {}

    ;
    Initialize.value = new Initialize();
    return Initialize;
  }();

  var HandleForm = function () {
    function HandleForm(value0) {
      this.value0 = value0;
    }

    ;

    HandleForm.create = function (value0) {
      return new HandleForm(value0);
    };

    return HandleForm;
  }();

  var LogUserOut = function () {
    function LogUserOut() {}

    ;
    LogUserOut.value = new LogUserOut();
    return LogUserOut;
  }();

  var newtypeSettingsForm = new Data_Newtype.Newtype(function (n) {
    return n;
  }, SettingsForm);

  var component = function component(dictMonadAff) {
    return function (dictNavigate) {
      return function (dictMonadAsk) {
        return function (dictManageUser) {
          var render = function render(v) {
            var formComponent = function () {
              var renderForm = function renderForm(v1) {
                var proxies = Formless_Transform_Row.mkSProxies()(newtypeSettingsForm)(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "bio";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "image";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))()(Formless_Transform_Row.makeSProxiesCons(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))()(Formless_Transform_Row.makeSProxiesNil))))))(Formless_Types_Form.FormProxy.value);
                var username = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))(newtypeSettingsForm)(newtypeSettingsForm)()()(proxies.username)(v1.form)([Halogen_HTML_Properties.placeholder("Your name"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputText.value)]);
                var password = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))(newtypeSettingsForm)(newtypeSettingsForm)()()(proxies.password)(v1.form)([Halogen_HTML_Properties.placeholder("Password"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputPassword.value)]);
                var image = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "image";
                }))(newtypeSettingsForm)(newtypeSettingsForm)()()(proxies.image)(v1.form)([Halogen_HTML_Properties.placeholder("URL of profile picture"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputText.value)]);
                var email = Rtsv2App_Form_Field.input(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))(newtypeSettingsForm)(newtypeSettingsForm)()()(proxies.email)(v1.form)([Halogen_HTML_Properties.placeholder("Email"), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputEmail.value)]);
                var bio = Halogen_HTML_Elements.fieldset([Rtsv2App_Component_HTML_Utils.css("form-group")])([Halogen_HTML_Elements.textarea([Rtsv2App_Component_HTML_Utils.css("form-control form-control-lg"), Halogen_HTML_Properties.placeholder("Short bio about you"), Halogen_HTML_Properties.rows(8), Halogen_HTML_Properties.value(Formless_Retrieve.getInput(new Data_Symbol.IsSymbol(function () {
                  return "bio";
                }))(newtypeSettingsForm)()(proxies.bio)(v1.form)), Halogen_HTML_Events.onValueInput(function () {
                  var $51 = Formless_Action.setValidate(new Data_Symbol.IsSymbol(function () {
                    return "bio";
                  }))(newtypeSettingsForm)()(proxies.bio);
                  return function ($52) {
                    return Data_Maybe.Just.create($51($52));
                  };
                }())])]);
                return Halogen_HTML_Elements.form_([Halogen_HTML_Elements.fieldset_([image, username, bio, email, password, Rtsv2App_Form_Field.submit("Update settings")])]);
              };

              var formInput = function formInput(v1) {
                return {
                  validators: {
                    image: Rtsv2App_Form_Validation.toOptional(Data_Monoid.monoidString)(Data_Eq.eqString)(dictManageUser.Monad0())(Rtsv2App_Form_Validation.avatarFormat(dictManageUser.Monad0())),
                    username: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictManageUser.Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.minLength(dictManageUser.Monad0())(3))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.maxLength(dictManageUser.Monad0())(20))(Rtsv2App_Form_Validation.usernameFormat(dictManageUser.Monad0())))),
                    bio: Formless_Validation.hoistFn_(dictManageUser.Monad0())(Control_Applicative.pure(Data_Maybe.applicativeMaybe)),
                    email: Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.required(Data_Eq.eqString)(Data_Monoid.monoidString)(dictManageUser.Monad0()))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.minLength(dictManageUser.Monad0())(3))(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.maxLength(dictManageUser.Monad0())(50))(Rtsv2App_Form_Validation.emailFormat(dictManageUser.Monad0())))),
                    password: Rtsv2App_Form_Validation.toOptional(Data_Monoid.monoidString)(Data_Eq.eqString)(dictManageUser.Monad0())(Control_Semigroupoid.composeFlipped(Formless_Validation.semigroupoidValidation(dictManageUser.Monad0()))(Rtsv2App_Form_Validation.minLength(dictManageUser.Monad0())(3))(Rtsv2App_Form_Validation.maxLength(dictManageUser.Monad0())(20)))
                  },
                  initialInputs: Data_Maybe.Nothing.value
                };
              };

              return Formless_Component.component(dictMonadAff)()()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))()(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))(Formless_Types_Form.eqInputField(Data_Eq.eqString)))(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.inputFieldsToFormFieldsNil)())())())())())(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.inputFieldsToInputCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.inputFieldsToInputNil)())())())())())(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.consCountErrors(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.nilCountErrors))))))(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.consAllTouched(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.nilAllTouched))))))(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.setFormFieldsTouchedNil)())())())())())(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.newtypeInputField)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.replaceFormFieldInputsTouchedNil))))))(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Types_Form.newtypeInputFunction)(Formless_Types_Form.newtypeFormField)()()()(Formless_Internal_Transform.modifyAllNil))))))(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))(dictManageUser.Monad0())()(newtypeSettingsForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(dictManageUser.Monad0())()(newtypeSettingsForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))(dictManageUser.Monad0())()(newtypeSettingsForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(dictManageUser.Monad0())()(newtypeSettingsForm)()()(Formless_Internal_Transform.applyToValidationCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(dictManageUser.Monad0())()(newtypeSettingsForm)()()(Formless_Internal_Transform.applyToValidationNil(dictManageUser.Monad0())))))))(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))()(Formless_Internal_Transform.formFieldsToMaybeOutputNil)())())())())())(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "bio";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "email";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "image";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "password";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowCons(new Data_Symbol.IsSymbol(function () {
                return "username";
              }))(Formless_Class_Initial.initialString)()(Formless_Transform_Row.mkInputFieldsFromRowNil)())())())())())(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)(newtypeSettingsForm)()()()()()()()()()(formInput)({
                render: renderForm,
                handleAction: Formless_Component.defaultSpec.handleAction,
                handleQuery: Formless_Component.defaultSpec.handleQuery,
                handleEvent: Formless_Component.raiseResult(newtypeSettingsForm)(Heterogeneous_Mapping.hmapRecord()(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "bio";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "email";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "image";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "password";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                  return "username";
                }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.unwrapField(Formless_Types_Form.newtypeOutputField)))(Heterogeneous_Mapping.mapRecordWithIndexNil)()())()())()())()())()())),
                receive: Formless_Component.defaultSpec.receive,
                initialize: Formless_Component.defaultSpec.initialize,
                finalize: Formless_Component.defaultSpec.finalize
              });
            }();

            var html = [Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-header")])([Halogen_HTML_Core.text("Your Settings")]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-content collapse show")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card-body")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "formless";
            }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(formComponent)(Data_Unit.unit)(function ($53) {
              return Data_Maybe.Just.create(HandleForm.create($53));
            }), Halogen_HTML_Elements.hr_, Halogen_HTML_Elements.button([Rtsv2App_Component_HTML_Utils.css("btn btn-outline-danger"), Halogen_HTML_Events.onClick(function (v1) {
              return new Data_Maybe.Just(LogUserOut.value);
            })])([Halogen_HTML_Core.text("Log out")])])])];
            return Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("main")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "header";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_Header.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Settings.value
            })(Data_Void.absurd), Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "mainMenu";
            }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Component_HTML_MainMenu.component(dictMonadAff)(dictNavigate))({
              currentUser: v.currentUser,
              route: Rtsv2App_Data_Route.Settings.value
            })(Data_Void.absurd), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("app-content content")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-wrapper-before")])([]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-header-left col-md-4 col-12 mb-2")])([Halogen_HTML_Elements.h3([Rtsv2App_Component_HTML_Utils.css("content-header-h3")])([Halogen_HTML_Core.text("Settings")])])]), Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("content-body")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("row")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("col-12")])([Halogen_HTML_Elements.div([Rtsv2App_Component_HTML_Utils.css("card")])(html)])])])])]), Rtsv2App_Component_HTML_Footer.footer]);
          };

          var initialState = function initialState(v) {
            return {
              currentUser: v.currentUser,
              profile: Network_RemoteData.NotAsked.value
            };
          };

          var handleAction = function handleAction(v) {
            if (v instanceof Initialize) {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Bind.bindFlipped(Halogen_Query_HalogenM.bindHalogenM)(function () {
                var $54 = Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()));
                return function ($55) {
                  return $54(Effect_Ref.read($55));
                };
              }())(Control_Monad_Reader_Class.asks(Halogen_Query_HalogenM.monadAskHalogenM(dictMonadAsk))(function (v1) {
                return v1.userEnv.currentUser;
              })))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (v2) {
                  var $33 = {};

                  for (var $34 in v2) {
                    if ({}.hasOwnProperty.call(v2, $34)) {
                      $33[$34] = v2[$34];
                    }

                    ;
                  }

                  ;
                  $33.currentUser = v1;
                  return $33;
                }))(function (v2) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                    var $37 = {};

                    for (var $38 in v3) {
                      if ({}.hasOwnProperty.call(v3, $38)) {
                        $37[$38] = v3[$38];
                      }

                      ;
                    }

                    ;
                    $37.profile = Network_RemoteData.Loading.value;
                    return $37;
                  }))(function () {
                    return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.getCurrentUser(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser)))(function (v3) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v4) {
                        var $41 = {};

                        for (var $42 in v4) {
                          if ({}.hasOwnProperty.call(v4, $42)) {
                            $41[$42] = v4[$42];
                          }

                          ;
                        }

                        ;
                        $41.profile = Network_RemoteData.fromMaybe(v3);
                        return $41;
                      }))(function () {
                        if (v3 instanceof Data_Maybe.Nothing) {
                          return Rtsv2App_Capability_Navigate.logout(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate));
                        }

                        ;

                        if (v3 instanceof Data_Maybe.Just) {
                          var newInputs = Formless_Transform_Record.wrapInputFields(newtypeSettingsForm)(Heterogeneous_Mapping.hmapRecord()(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                            return "bio";
                          }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.wrapField(Formless_Types_Form.newtypeInputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                            return "email";
                          }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.wrapField(Formless_Types_Form.newtypeInputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                            return "image";
                          }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.wrapField(Formless_Types_Form.newtypeInputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                            return "password";
                          }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.wrapField(Formless_Types_Form.newtypeInputField)))(Heterogeneous_Mapping.mapRecordWithIndexCons(new Data_Symbol.IsSymbol(function () {
                            return "username";
                          }))(Heterogeneous_Mapping.constMapping(Formless_Transform_Record.wrapField(Formless_Types_Form.newtypeInputField)))(Heterogeneous_Mapping.mapRecordWithIndexNil)()())()())()())()())()()))({
                            image: Data_Maybe.fromMaybe("")(Data_Functor.map(Data_Maybe.functorMaybe)(Rtsv2App_Data_Avatar.toString)(v3.value0.image)),
                            username: Rtsv2App_Data_Username.toString(v3.value0.username),
                            bio: Data_Maybe.fromMaybe("")(v3.value0.bio),
                            email: Data_Newtype.unwrap(Rtsv2App_Data_Email.newtypeEmail)(v3.value0.email),
                            password: ""
                          });
                          return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
                            return "formless";
                          }))(Data_Ord.ordUnit)(Formless_Types_Component["_formless"])(Data_Unit.unit)(Formless_Query.asQuery(Formless_Action.loadForm(newInputs))));
                        }

                        ;
                        throw new Error("Failed pattern match at Rtsv2App.Page.Settings (line 96, column 7 - line 107, column 77): " + [v3.constructor.name]);
                      });
                    });
                  });
                });
              });
            }

            ;

            if (v instanceof HandleForm) {
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.updateUser(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser))(v.value0))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Rtsv2App_Capability_Resource_User.getCurrentUser(Rtsv2App_Capability_Resource_User.manageUserHalogenM(dictManageUser)))(function (v1) {
                  return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v2) {
                    var $47 = {};

                    for (var $48 in v2) {
                      if ({}.hasOwnProperty.call(v2, $48)) {
                        $47[$48] = v2[$48];
                      }

                      ;
                    }

                    ;
                    $47.profile = Network_RemoteData.fromMaybe(v1);
                    return $47;
                  });
                });
              });
            }

            ;

            if (v instanceof LogUserOut) {
              return Rtsv2App_Capability_Navigate.logout(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate));
            }

            ;
            throw new Error("Failed pattern match at Rtsv2App.Page.Settings (line 86, column 18 - line 114, column 25): " + [v.constructor.name]);
          };

          return Component_HOC_Connect.component(dictMonadAff)(dictMonadAsk)()(Halogen_Component.mkComponent({
            initialState: initialState,
            render: render,
            "eval": Halogen_Component.mkEval({
              handleAction: handleAction,
              handleQuery: Halogen_Component.defaultEval.handleQuery,
              receive: Halogen_Component.defaultEval.receive,
              initialize: new Data_Maybe.Just(Initialize.value),
              finalize: Halogen_Component.defaultEval.finalize
            })
          }));
        };
      };
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Rtsv2App.Component.Router"] = $PS["Rtsv2App.Component.Router"] || {};
  var exports = $PS["Rtsv2App.Component.Router"];
  var Component_HOC_Connect = $PS["Component.HOC.Connect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Effect_Class = $PS["Effect.Class"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Hash = $PS["Routing.Hash"];
  var Rtsv2App_Capability_Navigate = $PS["Rtsv2App.Capability.Navigate"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Page_Home = $PS["Rtsv2App.Page.Home"];
  var Rtsv2App_Page_Login = $PS["Rtsv2App.Page.Login"];
  var Rtsv2App_Page_Profile = $PS["Rtsv2App.Page.Profile"];
  var Rtsv2App_Page_Register = $PS["Rtsv2App.Page.Register"];
  var Rtsv2App_Page_Settings = $PS["Rtsv2App.Page.Settings"];

  var Navigate = function () {
    function Navigate(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Navigate.create = function (value0) {
      return function (value1) {
        return new Navigate(value0, value1);
      };
    };

    return Navigate;
  }();

  var Initialize = function () {
    function Initialize() {}

    ;
    Initialize.value = new Initialize();
    return Initialize;
  }();

  var Receive = function () {
    function Receive(value0) {
      this.value0 = value0;
    }

    ;

    Receive.create = function (value0) {
      return new Receive(value0);
    };

    return Receive;
  }();

  var component = function component(dictMonadAff) {
    return function (dictMonadAsk) {
      return function (dictNow) {
        return function (dictLogMessages) {
          return function (dictNavigate) {
            return function (dictManageUser) {
              var handleQuery = function handleQuery(v) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Applicative.when(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Eq.notEq(Data_Maybe.eqMaybe(Rtsv2App_Data_Route.eqRoute))(v1.route)(new Data_Maybe.Just(v.value0)))(function () {
                    var v2 = Data_Maybe.isJust(v1.currentUser) && Data_Foldable.elem(Data_Foldable.foldableArray)(Rtsv2App_Data_Route.eqRoute)(v.value0)([Rtsv2App_Data_Route.Login.value, Rtsv2App_Data_Route.Register.value]);

                    if (!v2) {
                      return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v3) {
                        var $19 = {};

                        for (var $20 in v3) {
                          if ({}.hasOwnProperty.call(v3, $20)) {
                            $19[$20] = v3[$20];
                          }

                          ;
                        }

                        ;
                        $19.route = new Data_Maybe.Just(v.value0);
                        return $19;
                      });
                    }

                    ;
                    return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
                  }()))(function () {
                    return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value1));
                  });
                });
              };

              var handleAction = function handleAction(v) {
                if (v instanceof Initialize) {
                  return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Data_Functor.map(Halogen_Query_HalogenM.functorHalogenM)(function () {
                    var $44 = Routing_Duplex.parse(Rtsv2App_Data_Route.routeCodec);
                    return function ($45) {
                      return Data_Either.hush($44($45));
                    };
                  }())(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Routing_Hash.getHash)))(function (v1) {
                    return Rtsv2App_Capability_Navigate.navigate(Rtsv2App_Capability_Navigate.navigateHalogenM(dictNavigate))(Data_Maybe.fromMaybe(Rtsv2App_Data_Route.Home.value)(v1));
                  });
                }

                ;

                if (v instanceof Receive) {
                  return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                    var $28 = {};

                    for (var $29 in v1) {
                      if ({}.hasOwnProperty.call(v1, $29)) {
                        $28[$29] = v1[$29];
                      }

                      ;
                    }

                    ;
                    $28.currentUser = v.value0.currentUser;
                    return $28;
                  });
                }

                ;
                throw new Error("Failed pattern match at Rtsv2App.Component.Router (line 78, column 18 - line 86, column 48): " + [v.constructor.name]);
              };

              var authorize = function authorize(mbProfile) {
                return function (html) {
                  if (mbProfile instanceof Data_Maybe.Nothing) {
                    return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "login";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Login.component(dictMonadAff)(dictNavigate)(dictManageUser))({
                      redirect: false
                    })(Data_Void.absurd);
                  }

                  ;

                  if (mbProfile instanceof Data_Maybe.Just) {
                    return html;
                  }

                  ;
                  throw new Error("Failed pattern match at Rtsv2App.Component.Router (line 104, column 30 - line 108, column 11): " + [mbProfile.constructor.name]);
                };
              };

              var render = function render(v) {
                if (v.route instanceof Data_Maybe.Just) {
                  if (v.route.value0 instanceof Rtsv2App_Data_Route.Home) {
                    return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "home";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Home.component(dictMonadAff)(dictMonadAsk)(dictNavigate))({})(Data_Void.absurd);
                  }

                  ;

                  if (v.route.value0 instanceof Rtsv2App_Data_Route.Login) {
                    return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "login";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Login.component(dictMonadAff)(dictNavigate)(dictManageUser))({
                      redirect: true
                    })(Data_Void.absurd);
                  }

                  ;

                  if (v.route.value0 instanceof Rtsv2App_Data_Route.Register) {
                    return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "register";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Register.component(dictMonadAff)(dictManageUser)(dictMonadAsk)(dictNavigate))({})(Data_Void.absurd);
                  }

                  ;

                  if (v.route.value0 instanceof Rtsv2App_Data_Route.Settings) {
                    return authorize(v.currentUser)(Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "settings";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Settings.component(dictMonadAff)(dictNavigate)(dictMonadAsk)(dictManageUser))({})(Data_Void.absurd));
                  }

                  ;

                  if (v.route.value0 instanceof Rtsv2App_Data_Route.Profile) {
                    return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "profile";
                    }))(Data_Ord.ordUnit)(Data_Symbol.SProxy.value)(Data_Unit.unit)(Rtsv2App_Page_Profile.component(dictMonadAff)(dictNavigate)(dictMonadAsk)(dictManageUser))({
                      username: v.route.value0.value0
                    })(Data_Void.absurd);
                  }

                  ;
                  throw new Error("Failed pattern match at Rtsv2App.Component.Router (line 112, column 15 - line 123, column 83): " + [v.route.value0.constructor.name]);
                }

                ;

                if (v.route instanceof Data_Maybe.Nothing) {
                  return Halogen_HTML_Elements.div_([Halogen_HTML_Core.text("Oh no! That page wasn't found.")]);
                }

                ;
                throw new Error("Failed pattern match at Rtsv2App.Component.Router (line 111, column 35 - line 125, column 59): " + [v.route.constructor.name]);
              };

              return Component_HOC_Connect.component(dictMonadAff)(dictMonadAsk)()(Halogen_Component.mkComponent({
                initialState: function initialState(v) {
                  return {
                    route: Data_Maybe.Nothing.value,
                    currentUser: v.currentUser
                  };
                },
                render: render,
                "eval": Halogen_Component.mkEval({
                  handleAction: handleAction,
                  handleQuery: handleQuery,
                  receive: function receive($46) {
                    return Data_Maybe.Just.create(Receive.create($46));
                  },
                  initialize: new Data_Maybe.Just(Initialize.value),
                  finalize: Halogen_Component.defaultEval.finalize
                })
              }));
            };
          };
        };
      };
    };
  };

  exports["Navigate"] = Navigate;
  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.5
  "use strict";

  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Affjax = $PS["Affjax"];
  var Affjax_ResponseFormat = $PS["Affjax.ResponseFormat"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Argonaut_Decode_Class = $PS["Data.Argonaut.Decode.Class"];
  var Data_Argonaut_Decode_Struct_Tolerant_DecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.DecodeJson"];
  var Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson = $PS["Data.Argonaut.Decode.Struct.Tolerant.GDecodeJson"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Operator_Top = $PS["Data.Operator.Top"];
  var Data_Struct_Insert_RInsert = $PS["Data.Struct.Insert.RInsert"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_Bus = $PS["Effect.Aff.Bus"];
  var Effect_Aff_Class = $PS["Effect.Aff.Class"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Util = $PS["Halogen.Aff.Util"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_Query = $PS["Halogen.Query"];
  var Halogen_VDom_Driver = $PS["Halogen.VDom.Driver"];
  var Record_Builder = $PS["Record.Builder"];
  var Routing_Duplex = $PS["Routing.Duplex"];
  var Routing_Hash = $PS["Routing.Hash"];
  var Rtsv2App_Api_Endpoint = $PS["Rtsv2App.Api.Endpoint"];
  var Rtsv2App_Api_Request = $PS["Rtsv2App.Api.Request"];
  var Rtsv2App_AppM = $PS["Rtsv2App.AppM"];
  var Rtsv2App_Component_Router = $PS["Rtsv2App.Component.Router"];
  var Rtsv2App_Data_Avatar = $PS["Rtsv2App.Data.Avatar"];
  var Rtsv2App_Data_Route = $PS["Rtsv2App.Data.Route"];
  var Rtsv2App_Data_Username = $PS["Rtsv2App.Data.Username"];
  var Rtsv2App_Data_Utils = $PS["Rtsv2App.Data.Utils"];
  var Rtsv2App_Env = $PS["Rtsv2App.Env"];
  var Type_Equality = $PS["Type.Equality"];
  var Type_Proxying_Symbol = $PS["Type.Proxying.Symbol"];
  var main = Halogen_Aff_Util.runHalogenAff(Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.awaitBody)(function (v) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](Data_Maybe.Nothing.value)))(function (v1) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Aff_Bus.make(Effect_Class.monadEffectEffect)))(function (v2) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Rtsv2App_Api_Request.readToken))(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(function (token) {
          var requestOptions = {
            endpoint: Rtsv2App_Api_Endpoint.User.value,
            method: Rtsv2App_Api_Request.Get.value
          };
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_Class.liftAff(Effect_Aff_Class.monadAffAff)(Affjax.request(Rtsv2App_Api_Request.defaultRequest("https://conduit.productionready.io")(new Data_Maybe.Just(token))(requestOptions))))(function (v3) {
            var u = Control_Bind.bindFlipped(Data_Either.bindEither)(Rtsv2App_Data_Utils.decodeAt(Data_Argonaut_Decode_Struct_Tolerant_DecodeJson.decodeJsonRecord(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Data_Argonaut_Decode_Class.decodeJsonString))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_Plus()(Data_Argonaut_Decode_Class.decodeJsonMaybe(Rtsv2App_Data_Avatar.decodeJsonAvatar))(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_ConsNilCons_nonPlus()(Rtsv2App_Data_Username.decodeJsonUsername)(Data_Argonaut_Decode_Struct_Tolerant_GDecodeJson.gDecodeJson_NilNilNil(Record_Builder.categoryBuilder)(Data_Operator_Top.top1_Either))(new Data_Symbol.IsSymbol(function () {
              return "username";
            }))()(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
              return "username";
            }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
              return "image";
            }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
              return "image";
            }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))(new Data_Symbol.IsSymbol(function () {
              return "bio";
            }))()(Data_Maybe.plusMaybe)(Data_Struct_Insert_RInsert.rinsertBuilder(new Data_Symbol.IsSymbol(function () {
              return "bio";
            }))(Type_Proxying_Symbol.sProxyingSProxy))(Record_Builder.semigroupoidBuilder))())("user"))(Data_Bifunctor.lmap(Data_Either.bifunctorEither)(Affjax_ResponseFormat.printResponseFormatError)(v3.body));
            return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write(Data_Either.hush(u))(v1));
          });
        })))(function () {
          var environment = function () {
            var userEnv = {
              currentUser: v1,
              userBus: v2
            };
            return {
              baseUrl: "https://conduit.productionready.io",
              logLevel: Rtsv2App_Env.Dev.value,
              userEnv: userEnv
            };
          }();

          var rootComponent = Halogen_Component.hoist(Halogen_HTML_Core.bifunctorHTML)(Effect_Aff.functorAff)(Rtsv2App_AppM.runAppM(environment))(Rtsv2App_Component_Router.component(Rtsv2App_AppM.monadAffAppM)(Rtsv2App_AppM.monadAskAppM(Type_Equality.refl))(Rtsv2App_AppM.nowAppM)(Rtsv2App_AppM.logMessagesAppM)(Rtsv2App_AppM.navigateAppM)(Rtsv2App_AppM.manageUserAppM));
          return Control_Bind.bind(Effect_Aff.bindAff)(Halogen_VDom_Driver.runUI(rootComponent)({})(v))(function (v3) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Functor["void"](Effect_Aff.functorAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Routing_Hash.matchesWith(Data_Either.foldableEither)(Routing_Duplex.parse(Rtsv2App_Data_Route.routeCodec))(function (old) {
              return function ($$new) {
                return Control_Applicative.when(Effect.applicativeEffect)(Data_Eq.notEq(Data_Maybe.eqMaybe(Rtsv2App_Data_Route.eqRoute))(old)(new Data_Maybe.Just($$new)))(Effect_Aff.launchAff_(v3.query(Halogen_Query.tell(Rtsv2App_Component_Router.Navigate.create($$new)))));
              };
            }))))(function () {
              return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit);
            });
          });
        });
      });
    });
  }));
  exports["main"] = main;
})(PS);

PS["Main"].main();
},{"process":"../../../../../.nvm/versions/node/v13.5.0/lib/node_modules/parcel-bundler/node_modules/process/browser.js"}],"../../../../../.nvm/versions/node/v13.5.0/lib/node_modules/parcel-bundler/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "62679" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else if (location.reload) {
        // `location` global exists in a web worker context but lacks `.reload()` function.
        location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["../../../../../.nvm/versions/node/v13.5.0/lib/node_modules/parcel-bundler/src/builtins/hmr-runtime.js","rtsv2AppBundle.js"], null)
//# sourceMappingURL=/rtsv2AppBundle.js.map