exports.eqUUIDImpl = function(lhs) {
  return function(rhs) {
    return lhs == rhs
  }
}
exports.compareUUIDImpl = function(lt) {
  return function(eq) {
    return function(gt) {
      return function(lhs) {
        return function(rhs) {
          if (lhs < rhs) {
            return lt;
          }
          else if (lhs == rhs) {
            return eq;
          }
          else {
            return gt;
          }
        }
      }
    }
  }
}
exports.stringToUUIDImpl = function(nothing) {
  return function(just) {
    return function(str) {
      if ((/^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/).test(str)) {
        return just(str);
      }
      else {
        return nothing;
      }
    }
  }
}
exports.uuidToStringImpl = function(uuid) {
  return uuid;
}
exports.emptyImpl = function() {
  return "00000000-0000-0000-0000-000000000000";
}
