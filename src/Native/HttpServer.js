var _JeffHoremans$elm_multitier$Native_HttpServer = function() {

  var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';
  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;

  if(isNode){

      var http = require('http');
      var url = require('url');
      var fs = require('fs');

      var listen = function (port, settings) {
        return Scheduler.nativeBinding(function (callback) {

          var server = http.createServer();

          server.on('listening', function () {
            console.log('server listening on ', server.address());
            callback(_elm_lang$core$Native_Scheduler.succeed(server));
          });

          server.on('request', function (req, res) {

            var request = {
              request: req,
              response: res
            };
            Scheduler.rawSpawn(settings.onRequest(request));
          });

          server.on('close', function () {
            console.log('server closed');
            Scheduler.rawSpawn(settings.onClose());
          });

          server.listen(port);

          return function () {
            server.close();
          };
        });
      }

      var reply = function(request, string) {
        return Scheduler.nativeBinding(function (callback) {
          request.response.end(string);

          callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        });
      }

      var replyFile = function(request, filename) {
        return Scheduler.nativeBinding(function(callback) {

          fs.readFile(filename,function (err, data){
            if(err) {
              request.response.end()
              callback(Scheduler.fail(Utils.Tuple0));
            } else {
              // res.writeHead(200, {'Content-Type': 'text/html','Content-Length':data.length});
              request.response.write(data);
              request.response.end();
              callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
            }
          });
        })
      }

      var getPath = function(req) {
        return url.parse(req.request.url).path;
      }

      var close = function(server) {
        return Scheduler.nativeBinding(function (callback) {

          server.close();

          callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        });
      }

      return {
        listen: F2(listen),
        reply: F2(reply),
        replyFile: F2(replyFile),
        getPath: getPath,
        close: close
      };
  } else {

      var listen = function(port, settings) {
        return Scheduler.nativeBinding(function(callback) {
          return callback(Scheduler.fail(Utils.Tuple0));
        });
      }

      var reply = function(request, string) {
        return Scheduler.nativeBinding(function(callback) {
          return callback(Scheduler.fail(Utils.Tuple0));
        });
      }

      var replyFile = function(request, filename) {
        return Scheduler.nativeBinding(function(callback) {
          return callback(Scheduler.fail(Utils.Tuple0));
        });
      }

      var getPath = function(req) {
        return ""
      }

      var close = function(server) {
        return Scheduler.nativeBinding(function(callback) {
          return callback(Scheduler.fail(Utils.Tuple0));
        });
      }

      return {
        listen: F2(listen),
        reply: F2(reply),
        replyFile: F2(replyFile),
        getPath : getPath,
        close: close
      };

  }

}();

(function() {

    var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';

    if (isNode) {
      setTimeout(function() {
          if (!module.parent) {
              if ('ServerStarter' in module.exports) {
                  module.exports.ServerStarter.worker();
              } else {
                  throw new Error('TODO: error message.');
              }
          }
      });
    } else {
      return;
    }
})();
