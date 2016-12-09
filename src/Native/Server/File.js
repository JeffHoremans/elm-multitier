var _JeffHoremans$elm_multitier$Native_Server_File = (function() {

  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;
  var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';


  if(isNode){
    var fs = require('fs');

    var read = function(path) {
      return  Scheduler.nativeBinding(function(callback) {
        fs.readFile(path, 'utf8', function(err, data) {
          if (err) {
            return callback(Scheduler.fail('ReadError'));
          }
          return callback(Scheduler.succeed(data));
        });
      });
    }

    var write = function(path, data) {
      return Scheduler.nativeBinding(function(callback) {
        fs.writeFile(path, data, function(err) {
          if (err) {
            return callback(Scheduler.fail('WriteError'));
          }
          return callback(Scheduler.succeed(Utils.Tuple0));
        });
      });
    }

    return {
      read: read,
      write: F2(write)
    };
  } else {

  	var read = function(path) {
  		throw new Error("Not on Node environment")
  	}

  	var write = function(path, data) {
  		throw new Error("Not on Node environment")
  	}

  	return {
  		read: read,
  		write: F2(write)
    };

  }




})();
