var _user$project$Native_Multitier_Server_HttpServer_LowLevel = function() {

  var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';
  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;

  if(isNode){

      var http = require('http');
      var url = require('url');
      var fs = require('fs');
      var WebSocketServer = require('websocket').server;
      var connection_id = 0
      var active_connections = {}

      var listen = function (port, handlers) {
        return Scheduler.nativeBinding(function (callback) {

          var server = http.createServer();

          server.on('listening', function () {
            console.log('server listening on ', server.address());
            callback(_elm_lang$core$Native_Scheduler.succeed(server));
          });

          server.on('request', function (req, res) {
            var fullBody = '';
            var rawRequest = {
              request: req,
              response: res
            };


            req.on('data', function(chunk) {
               fullBody += chunk.toString();
            });

            req.on('end', function() {
              var request = { method: { ctor: req.method }
                            , path: url.parse(req.url).path
                            , body: fullBody
                            , rawRequest: rawRequest }
              Scheduler.rawSpawn(handlers.onRequest(request));
            });
          });

          server.on('close', function () {
            console.log('server closed');
            Scheduler.rawSpawn(handlers.onClose());
          });

          server.listen(port);

          return function () {
            server.close();
          };
        });
      }

      var openSocket = function(server, handlers){
        return Scheduler.nativeBinding(function(callback) {
          console.log("WebSocket Server started...")
          var wsServer = new WebSocketServer({
              httpServer: server,
              // You should not use autoAcceptConnections for production
              // applications, as it defeats all standard cross-origin protection
              // facilities built into the protocol and the browser.  You should
              // *always* verify the connection's origin and decide whether or not
              // to accept it.
              autoAcceptConnections: false
          });

          var originIsAllowed = function(origin) {
            // put logic here to detect whether the specified origin is allowed.
            return true;
          }

          wsServer.on('request', function(request) {
              if (!originIsAllowed(request.origin)) {
                // Make sure we only accept requests from an allowed origin
                request.reject();
                console.log((new Date()) + ' Connection from origin ' + request.origin + ' rejected.');
                return;
              }

              var connection = request.accept(null, request.origin);
              var id = connection_id++;
              active_connections[id] = connection;
              connection.id = id

              connection.on('message', function(message){
                if(message.type === 'utf8'){
                  Scheduler.rawSpawn(handlers.onMessage({clientId: id, data: message.utf8Data}));
                }
                else if(message.type === 'binary'){
                  request.reject()
                }

              })

              connection.on('close', function(reasonCode, description) {
                  console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
                  delete active_connections[connection.id]
              });
          });

          callback(_elm_lang$core$Native_Scheduler.succeed({ ctor:"SocketServer" }))

        })
      }

      var broadcast = function(server, message) {
        return Scheduler.nativeBinding(function(callback) {
          for (var id in active_connections){
            active_connections[id].sendUTF(message)
          }
          callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        })
      }

      var send = function(server, cid, message) {
        return Scheduler.nativeBinding(function(callback) {
          if(active_connections[cid]){
            active_connections[cid].sendUTF(message)
            callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
          } else {
            callback(_elm_lang$core$Native_Scheduler.fail("Client with the given id is not connected (anymore)..."));
          }
        })
      }

      var reply = function(request, string) {
        return Scheduler.nativeBinding(function (callback) {
          request.rawRequest.response.end(string);
          callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        });
      }

      var replyFile = function(request, filename) {
        return Scheduler.nativeBinding(function(callback) {
          try {
            var readStream = fs.createReadStream(__dirname + '/' + filename);
            readStream.pipe(request.rawRequest.response);
            callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
          } catch (err) {
            request.rawRequest.response.end()
            callback(Scheduler.fail(Utils.Tuple0));
          }
        })
      }

      return {
        listen: F2(listen),
        reply: F2(reply),
        replyFile: F2(replyFile),
        openSocket: F2(openSocket),
        broadcast: F2(broadcast),
        send: F3(send)
      };
  } else {

      var listen = function(port, handlers) {
        throwError()
      }

      var reply = function(request, string) {
        throwError()
      }

      var replyFile = function(request, filename) {
        throwError()
      }

      var openSocket = function(server, handlers) {
        throwError()
      }

      var broadcast = function(server, message) {
        throwError()
      }

      var send = function(server, cid, message) {
        throwError()
      }

      var close = function(server) {
        throwError()
      }

      var throwError = function(){
        throw new Error("Not on Node environment!")
      }

      return {
        listen: F2(listen),
        reply: F2(reply),
        replyFile: F2(replyFile),
        openSocket: F2(openSocket),
        broadcast: F2(broadcast),
        send: F3(send)
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
