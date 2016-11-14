var _JeffHoremans$elm_multitier$Native_Multitier = (function() {

  var toJSON = function(x) {
      try {
        console.log("toJSON")
        console.log(x)
        var object = x
        JSON.stringify(object)
        return object;
      } catch (err) {
        throw new Error("Report this issue, this should never happen! Failed to encode Elm data to JSON: " + err.message)
      }
  }

  var fromJSON = function(x) {
      console.log("fromJSON")
      console.log(x)
      try {
        if (typeof(x) === 'string') {
          return JSON.parse(x);
        } else {
          return x;
        }
      } catch (err) {
        throw new Error("Report this issue, this should never happen! Failed to decode JSON to Elm data: " + err.message)
      }

  }

  return {
    toJSON: toJSON,
    fromJSON: fromJSON
  };

})();
