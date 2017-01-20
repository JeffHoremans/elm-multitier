var _user$project$Native_Multitier_Server_File = (function() {

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

    var exists = function(path) {
      return fs.existsSync(path)
    }

    return {
      read: read,
      write: F2(write),
      exists: exists
    };
  } else {

  	var read = function(path) {
  		throwError()
  	}

  	var write = function(path, data) {
  		throwError()
  	}

    var exists = function(path){
      throwError()
    }

    var throwError = function(){
      throw new Error("Not on Node environment!")
    }

  	return {
  		read: read,
  		write: F2(write),
      exists: exists
    };

  }




})();
