'use strict';
document.addEventListener('DOMContentLoaded', () => {
  const app = Elm.Main.init({
    node: document.getElementById('app'),
  });

  // Mostly copied from: the elm websocket library's README:
  //
  // https://package.elm-lang.org/packages/bburdette/websocket/latest/
  const mySockets = {};
  function sendSocketCommand(wat) {
    if (wat.cmd == "connect")
    {
      let scheme;
      switch(window.location.protocol) {
        case "http:":
          scheme = 'ws';
          break;
        case "https:":
          scheme = 'wss';
          break;
        default:
          throw("Not running over http; can't infer websocket scheme.");
      }
      const addr = scheme + "://" + window.location.host;
      const socket = new WebSocket(addr, 'dndgrid');
      socket.onmessage = function (event) {
        app.ports.receiveSocketMsg.send({ name : wat.name
                                        , msg : "data"
                                        , data : event.data} );
      }
      mySockets[wat.name] = socket;
    }
    else if (wat.cmd == "send")
    {
      mySockets[wat.name].send(wat.content);
    }
    else if (wat.cmd == "close")
    {
      mySockets[wat.name].close();
      delete mySockets[wat.name];
    }
  }
  app.ports.sendSocketCommand.subscribe(sendSocketCommand);
});
