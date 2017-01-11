var _JeffHoremans$elm_multitier$Native_Multitier_Server_Console = (function() {

  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;

  var log = function(value) {
    return Scheduler.nativeBinding(function(callback) {
      if (typeof value == "string") {
        console.log(value);
      } else {
        console.log(Utils.toString(value));
      }
      return callback(Scheduler.succeed(Utils.Tuple0));
    });
  }

  return {
    log: log
  };

})();
