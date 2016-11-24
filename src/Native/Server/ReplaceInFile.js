var _JeffHoremans$elm_multitier$Native_Server_ReplaceInFile = (function() {

  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;
  var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';

  if (isNode){
    var rif = require('replace-in-file');

    var replace = function(file, regex, replacement, allowEmptyPaths) {
      return Scheduler.nativeBinding(function(callback) {
        var options = {
          files:file,
          replace: new RegExp(regex),
          with: replacement,
          allowEmptyPaths: allowEmptyPaths,
        };
        console.log(options)
        try {
          rif.sync(options);
          return callback(Scheduler.succeed(Utils.Tuple0));
        }
        catch (error) {
          return callback(Scheduler.succeed("Error occurred while replacing in file: " + error));
        }
      });
    }

    return {
      replace: F4(replace)
    };
  } else {

    var replace = function(file,regex,replacement,allowEmptyPaths) {
      return Scheduler.nativeBinding(function(callback) {
        return calllback(Scheduler.fail("Not on Node environment!"))
      })
    }

    return {
      replace: F4(replace)
    }

  }

})();
