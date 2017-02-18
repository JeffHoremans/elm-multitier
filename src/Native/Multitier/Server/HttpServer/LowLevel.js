var _user$project$Native_Multitier_Server_HttpServer_LowLevel = function() {

  var isNode = typeof global !== "undefined" && ({}).toString.call(global) === '[object global]';
  var Utils =  _elm_lang$core$Native_Utils;
  var Scheduler = _elm_lang$core$Native_Scheduler;

  if(isNode){

      var http = require('http');
      var url = require('url');
      var fs = require('fs');
      var WebSocketServer = require('websocket').server;
      var WebSocketRouter = require('websocket').router;

      var connection_ids = {}
      var active_connections = {}
      var mounted = {}

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

      var createSocketRouter = function(server){
        return Scheduler.nativeBinding(function(callback) {
          var wsServer = new WebSocketServer({
              httpServer: server,
              // You should not use autoAcceptConnections for production
              // applications, as it defeats all standard cross-origin protection
              // facilities built into the protocol and the browser.  You should
              // *always* verify the connection's origin and decide whether or not
              // to accept it.
              autoAcceptConnections: false
          });

          var router = new WebSocketRouter();
          router.attachServer(wsServer);
          callback(_elm_lang$core$Native_Scheduler.succeed(router))
        });
      }

      var openSocket = function(router, path, handlers){
        return Scheduler.nativeBinding(function(callback){
          console.log("Opening socket with path: " + path);
          var originIsAllowed = function(origin) {
            // put logic here to detect whether the specified origin is allowed.
            // Could include this in the handlers in the future
            return true;
          }

          if (!connection_ids[path]) connection_ids[path] = 0;
          if (!active_connections[path]) active_connections[path] = {};
          mounted[path] = true;

          router.mount(path, null, function(request) {

            if (!originIsAllowed(request.origin)) {
              // Make sure we only accept requests from an allowed origin
              request.reject();
              console.log((new Date()) + ' Connection from origin ' + request.origin + ' rejected.');
              return;
            }

            var connection = request.accept(request.origin);
            var id = connection_ids[path]++;
            active_connections[path][id] = connection;
            connection.id = id
            Scheduler.rawSpawn(handlers.onConnect({ ctor: "ClientId", _0:path, _1: id}));

            connection.on('message', function(message){
              if(message.type === 'utf8'){
                Scheduler.rawSpawn(handlers.onMessage({clientId: { ctor: "ClientId", _0:path, _1: id}, data: message.utf8Data}));
              }
              else if(message.type === 'binary'){
                request.reject()
              }

            })

            connection.on('close', function(reasonCode, description) {
                console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
                Scheduler.rawSpawn(handlers.onDisconnect({ ctor: "ClientId", _0:path, _1: id}));
                delete active_connections[path][id]
            });

          });
          callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing))
        });
      }

      var closeSocket = function(path, socket){
        return Scheduler.nativeBinding(function(callback){
          router.unmount(path, '____no_protocol____');
          mounted[path] = false;
          console.log("Closed socket with path " + path );
          callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        });
      }

      var broadcast = function(path, message) {
        console.log(path);
        console.log(message);
        return Scheduler.nativeBinding(function(callback) {
          if(mounted[path] === true){
            for (var id in active_connections[path]){
              active_connections[path][id].sendUTF(message);
            }
          }
          callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
        })
      }

      var send = function(path, cid, message) {
        return Scheduler.nativeBinding(function(callback) {
          if (path === cid._0){
            if(mounted[path] === true){
              if(active_connections[path][cid._1]){
                active_connections[path][cid._1].sendUTF(message)
                callback(Scheduler.succeed(_elm_lang$core$Maybe$Nothing));
              } else {
                callback(Scheduler.fail("Client with the given id is not connected (anymore)..."));
              }
            } else {
              callback(Scheduler.fail("The given socket is closed..."));
            }
          } else {
            callback(Scheduler.fail("The given client id does not belong to the given socket..."))
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
        createSocketRouter: createSocketRouter,
        openSocket: F3(openSocket),
        closeSocket: F2(closeSocket),
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

      var createSocketRouter = function(server) {
        throwError()
      }

      var openSocket = function(router, path, handlers) {
        throwError()
      }

      var closeSocket = function(router, socket) {
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
        createSocketRouter: createSocketRouter,
        openSocket: F3(openSocket),
        closeSocket: F2(closeSocket),
        broadcast: F2(broadcast),
        send: F3(send)
      };

  }

}();
