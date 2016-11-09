var _JeffHoremans$elm_multitier$Native_Multitier = (function() {

  var Ok = _elm_lang$core$Result$Ok
  var Err = _elm_lang$core$Result$Err

  var toJSON = function(x) {
      try {
        var object = x
        JSON.stringify(object)
        return Ok(object);
      } catch (err) {
        return Err(err.message)
      }
  }

  var fromJSON = function(x) {
      try {
        var object = JSON.parse(x)
        return Ok(object);
      } catch (err) {
        return Err(err.message);
      }

  }

  return {
    toJSON: toJSON,
    fromJSON: fromJSON
  };

})();
