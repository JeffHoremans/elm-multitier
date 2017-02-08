var _user$project$Native_Multitier_LowLevel = (function() {

  var toJSON = function(x) {
      try {
        var object = x
        JSON.stringify(object)
        return object;
      } catch (err) {
        throw new Error("Report this issue, this should never happen! Failed to encode Elm data to JSON: " + err.message)
      }
  }

  var fromJSON = function(x) {
      try {
        return x;
      } catch (err) {
        throw new Error("Report this issue, this should never happen! Failed to decode JSON to Elm data: " + err.message)
      }
  }

  var fromJSONString = function(x) {
      try {
        return JSON.parse(x);
      } catch (err) {
        throw new Error("Report this issue, this should never happen! Failed to decode JSON string to Elm data: " + err.message)
      }
  }

  return {
    toJSON: toJSON,
    fromJSON: fromJSON,
    fromJSONString: fromJSONString
  };

})();
